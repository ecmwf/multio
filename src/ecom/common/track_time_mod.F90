#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'track_time_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'TRACK_TIME_MOD'
MODULE TRACK_TIME_MOD

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,         ONLY: JPIB_K
  USE :: OM_CORE_MOD,         ONLY: JPRD_K
  USE :: MAP_MOD,             ONLY: MAP_T
  USE :: CIRCULAR_BUFFER_MOD, ONLY: CB_LIST_T
  USE :: CIRCULAR_BUFFER_MOD, ONLY: CIRCULARBUFFER_T

IMPLICIT NONE

! Deafualt visibility of the model
PRIVATE

! Capacity of the circular buffer that contains informaitons about the timing of each message
INTEGER(KIND=JPIB_K), PARAMETER :: CAPACITY=100

! RB-Trees used to track level for different fields
CLASS(MAP_T), TARGET, ALLOCATABLE, DIMENSION(:) :: TIMING

! List of circular buffers used to dealloca the memory
TYPE(CB_LIST_T) :: TDATA

! Time history datatype
TYPE :: TIME_HISTORY_T
  INTEGER(KIND=JPIB_K) :: SIZE_
  INTEGER(KIND=JPIB_K), DIMENSION(CAPACITY) :: HIST_
END TYPE


TYPE :: CURR_TIME_T
  INTEGER(KIND=JPIB_K) :: ISEC
  INTEGER(KIND=JPIB_K) :: ISEC0
  INTEGER(KIND=JPIB_K) :: ISTEP
  INTEGER(KIND=JPIB_K) :: ISTEP0
  REAL(KIND=JPRD_K)    :: TSTEP
END TYPE


! Whitelist of public symbols
PUBLIC :: TIME_HISTORY_T
PUBLIC :: CURR_TIME_T
PUBLIC :: SUTRAK_TIME
PUBLIC :: TRACK_TIME_ACCESS_OR_CREATE
PUBLIC :: TRACK_TIME_PRINT
PUBLIC :: TRACK_TIME_FREE
PUBLIC :: COMPUTE_CURRENT_TIME


CONTAINS

#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SUTRAK_TIME'
SUBROUTINE SUTRAK_TIME( CFG, MODEL_PARAMS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: LOOKUP_TABLE
  USE :: MAP_MOD,            ONLY: MAP_INIT

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FCKIT_CONFIGURATION), INTENT(IN) :: CFG
  TYPE(MODEL_PAR_T), TARGET, INTENT(IN) :: MODEL_PARAMS

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( .NOT. ALLOCATED(TIMING) ) THEN

   ALLOCATE( TIMING(SIZE(LOOKUP_TABLE)) )

   DO I = 1, SIZE(TIMING)
     CALL MAP_INIT( TIMING(I) )
   ENDDO

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SUTRAK_TIME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TRACK_TIME_ACCESS_OR_CREATE'
SUBROUTINE TRACK_TIME_ACCESS_OR_CREATE( GRIB_ID, LEVEL, STEP, TIME_HISTORY )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,         ONLY: JPIB_K
  USE :: GRIB_INFO_DATA_MOD,  ONLY: LOOKUP_TABLE
  USE :: MAP_MOD,             ONLY: MAP_INSERT
  USE :: MAP_MOD,             ONLY: MAP_GET
  USE :: MAP_MOD,             ONLY: KEY_T
  USE :: CIRCULAR_BUFFER_MOD, ONLY: CIRCULARBUFFER_T
  USE :: CIRCULAR_BUFFER_MOD, ONLY: CB_INIT
  USE :: CIRCULAR_BUFFER_MOD, ONLY: CB_GET_ALL
  USE :: CIRCULAR_BUFFER_MOD, ONLY: CB_ENQUEUE
  USE :: CIRCULAR_BUFFER_MOD, ONLY: CB_NEW

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: GRIB_ID
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: LEVEL
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: STEP
  TYPE(TIME_HISTORY_T),  INTENT(INOUT) :: TIME_HISTORY

  ! Local variables
  INTEGER(KIND=JPIB_K) :: IDX
  TYPE(KEY_T) :: KEY
  CLASS(CIRCULARBUFFER_T), POINTER :: CB
  CLASS(*), POINTER :: VALUE
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  CB => NULL()
  KEY%K = LEVEL
  IDX = LOOKUP_TABLE( GRIB_ID )
  EX = MAP_GET( TIMING(IDX), KEY, VALUE )

  IF ( EX ) THEN

    SELECT TYPE ( A => VALUE )

    CLASS IS ( CIRCULARBUFFER_T )

      CALL CB_ENQUEUE( A, STEP )

      EX = CB_GET_ALL( A, TIME_HISTORY%SIZE_, TIME_HISTORY%HIST_  )

    CLASS DEFAULT

    END SELECT

  ELSE

    ! CB_NEW return a newly allocated node, and internally push it into a list of allocated nodes,
    ! in this way it is possible to deallocate all the memory that has been allocated.
    ! The map jus contains abstract pointers hence it is not able to free memory
    CB => CB_NEW( TDATA, CAPACITY )
    CALL CB_INIT( CB, CAPACITY )
    CALL CB_ENQUEUE( CB, STEP )
    VALUE => CB
    CALL MAP_INSERT( TIMING(IDX), KEY, VALUE )
    EX = CB_GET_ALL( CB, TIME_HISTORY%SIZE_, TIME_HISTORY%HIST_  )

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE TRACK_TIME_ACCESS_OR_CREATE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'COMPUTE_CURRENT_TIME'
FUNCTION COMPUTE_CURRENT_TIME( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME ) RESULT(EX)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: JPRD_K
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,        ONLY: OM_BASE_MSG_A
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),    INTENT(IN)  :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),    INTENT(IN)  :: GRIB_INFO
  TYPE(TIME_HISTORY_T), INTENT(IN)  :: TIME_HIST
  TYPE(CURR_TIME_T),    INTENT(OUT) :: CURR_TIME

  ! Function result
  LOGICAL :: EX

  ! Local variables
  LOGICAL, DIMENSION(2) :: CONDITIONS
  INTEGER(KIND=JPIB_K)  :: ISTEP
  INTEGER(KIND=JPIB_K)  :: ISECSTART

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get current step
  ISTEP = TIME_HIST%HIST_(TIME_HIST%SIZE_)

  ! Compute Current time
  ! This must be moved somewhere else since it is shared between
  ! grib1 and grib2
  IF( MODEL_PARAMS%SIM_%LPPSTEPS ) THEN
    CURR_TIME%TSTEP = 3600._JPRD_K
    CURR_TIME%ISEC  = INT( REAL(ISTEP,JPRD_K)*CURR_TIME%TSTEP, JPIB_K)
  ELSE
    CURR_TIME%TSTEP = MODEL_PARAMS%SIM_%TSTEP
    CURR_TIME%ISEC  = INT( REAL(ISTEP,JPRD_K)*CURR_TIME%TSTEP, JPIB_K)
  ENDIF


  IF( TRIM(MODEL_PARAMS%SIM_%CTYPE) .EQ. 'fc' ) THEN
    ! This is a bug
    CURR_TIME%ISEC = CURR_TIME%ISEC + MODEL_PARAMS%SIM_%NSTEPINI*3600
  ENDIF
  CURR_TIME%ISEC0=0

  ISECSTART = CURR_TIME%ISEC - GRIB_INFO%OVERALL_LENGTH_OF_TIME_RANGE_*3600.0_JPRD_K

  CONDITIONS(1) = ISECSTART .LT. CURR_TIME%ISEC0
  CONDITIONS(2) = GRIB_INFO%IS_STEP_VALID0_

  EX = (ISECSTART .GE.CURR_TIME% ISEC0) .OR. ALL( CONDITIONS )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION COMPUTE_CURRENT_TIME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TRACK_TIME_PRINT'
SUBROUTINE TRACK_TIME_PRINT( TIME_HISTORY, LOGUNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(TIME_HISTORY_T), INTENT(INOUT) :: TIME_HISTORY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: LOGUNIT

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  WRITE(LOGUNIT,'(A)') ' '
  WRITE(LOGUNIT,'(A)') ' TIME HISTORY (steps)'
  WRITE(LOGUNIT,'(A)') ' --------------------'
  WRITE(LOGUNIT,'(A)', ADVANCE='NO') ' + '
  DO I = 1, TIME_HISTORY%SIZE_ - 1
    WRITE(LOGUNIT,'(I8,A)', ADVANCE='NO') TIME_HISTORY%HIST_(I), ', '
  ENDDO
  WRITE(LOGUNIT,'(I8)') TIME_HISTORY%HIST_(TIME_HISTORY%SIZE_)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE TRACK_TIME_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TRACK_TIME_FREE'
SUBROUTINE TRACK_TIME_FREE()
IMPLICIT NONE

! 1) Deallocate all the maps (this function uses the recursive procedure that uses a lot of stack,
!    for the future it is better to reimplement it using a loop)
! 2) Call CB_FREE to delete all the nodes in that have been allocated


END SUBROUTINE TRACK_TIME_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE TRACK_TIME_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME