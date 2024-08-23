#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'time_preset_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'TIME_PRESET_UTILS_MOD'
MODULE TIME_PRESET_UTILS_MOD

IMPLICIT NONE

! Default visibility
PRIVATE

! Whitelist of public symbols
PUBLIC :: TIME_PRESET_INIT
PUBLIC :: TIME_PRESET_FREE
PUBLIC :: TIME_PRESET_GRIBX_ATM

CONTAINS


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TIME_PRESET_INIT'
SUBROUTINE TIME_PRESET_INIT( CFG, MODEL_PARAMS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,  ONLY: MODEL_PAR_T

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FCKIT_CONFIGURATION), INTENT(IN) :: CFG
  TYPE(MODEL_PAR_T), TARGET, INTENT(IN) :: MODEL_PARAMS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE TIME_PRESET_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TIME_PRESET_FREE'
SUBROUTINE TIME_PRESET_FREE( )

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE TIME_PRESET_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Pre-sets the date and time of the simulation
!>
!> This function sets the time informations of a simulation into the provided metadata object
!> using information retrieved from the specified data structure (YDIOS).
!>
!> @param [inout] METADATA Metadata object where the GPI will be set.
!> @param [in]    YDIOS    Data structure used to retrieve the information necessary to compute the GPI.
!>
!> @todo Deprecation Notice: The functionality related to VAREPS is deprecated and will not be used in the future.
!>       All code related to VAREPS should be removed.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TIME_PRESET'
SUBROUTINE TIME_PRESET_GRIBX_ATM( MODEL_PARAMS, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A
  USE :: DATETIME_UTILS_MOD, ONLY: PACK_HHMM
  USE :: DATETIME_UTILS_MOD, ONLY: SEC2HH_MM_SS
  USE :: DATETIME_UTILS_MOD, ONLY: UNPACK_YYYYMMDD
  USE :: DATETIME_UTILS_MOD, ONLY: PACK_YYYYMMDD
  USE :: DATETIME_UTILS_MOD, ONLY: DATE_SUB_DAYS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables
  LOGICAL, DIMENSION(2) :: CONDITION1
  LOGICAL, DIMENSION(2) :: CONDITION2

  INTEGER(KIND=JPIB_K) :: DYYYY
  INTEGER(KIND=JPIB_K) :: DMM
  INTEGER(KIND=JPIB_K) :: DDD
  INTEGER(KIND=JPIB_K) :: THH
  INTEGER(KIND=JPIB_K) :: TMM
  INTEGER(KIND=JPIB_K) :: TSS

  INTEGER(KIND=JPIB_K) :: DYYYY1
  INTEGER(KIND=JPIB_K) :: DMM1
  INTEGER(KIND=JPIB_K) :: DDD1
  INTEGER(KIND=JPIB_K) :: THH1
  INTEGER(KIND=JPIB_K) :: TMM1
  INTEGER(KIND=JPIB_K) :: TSS1
  INTEGER(KIND=JPIB_K) :: IFCDA_INI
  INTEGER(KIND=JPIB_K) :: IFCHO_RES
  INTEGER(KIND=JPIB_K) :: IINDAT
  INTEGER(KIND=JPIB_K) :: ITIME

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )


  ASSOCIATE ( YPI => MODEL_PARAMS%SIM_ )

    ! Extract date/time components
    CALL UNPACK_YYYYMMDD( YPI%NINDAT, DYYYY, DMM, DDD )
    CALL SEC2HH_MM_SS( YPI%NSSSSS, THH, TMM, TSS )

    ! Initialization of the modified date/time
    ! NOTE: Apparently minutes and seconds are cut away in the grib encoding,
    !       not sure it is the correct way to proceed
    DYYYY1 = DYYYY
    DMM1   = DMM
    DDD1   = DDD
    THH1   = THH
    TMM1   = 0
    TSS1   = 0

    ! First special case
    CONDITION1(1) = (YPI%CTYPE .EQ. 'fc') ! 'type' is forecast (gribCode=9)
    CONDITION1(2) = (YPI%LOBSC1)          ! .T. = term of observations included in configuration 1

    ! Second special case
    CONDITION2(1) = (YPI%LVAREPS)     ! .T. when running with variable resolution
    CONDITION2(2) = (YPI%NLEG .GE. 2) ! current VAREPS leg number (eg 1(2) for the T399(T255) part of a T399-T255 VAREPS)


    ! If needed modify the time
    IF ( ALL(CONDITION1) ) THEN
      ! NOTE: This code works because NSTEPINI is supposed to be less than 24
      ! NSTEPINI: Initial step in hours for the initial conditions
      !           at the beginning of 4D-Var trajectory (usually 3 hours).
      !           It is used to update the step while saving the FCs along
      !           the first trajectory.
      THH1 = THH - YPI%NSTEPINI
      IF ( THH1 .LT. 0 ) THEN
        THH1 = THH1 + 24
        CALL DATE_SUB_DAYS( DYYYY, DMM, DDD, INT(-1,JPIB_K), DYYYY1, DMM1, DDD1 )
      ENDIF

    ELSEIF ( ALL(CONDITION2) ) THEN
      ! NFCHO_TRUNC_INI: forecast step used to define the ICs (ie NFCHO_TRUNC of previous VAREPS LEG)
      IFCDA_INI = YPI%NFCHO_TRUNC_INI/24
      IFCHO_RES = MOD(YPI%NFCHO_TRUNC_INI, 24)
      THH1 = THH - IFCHO_RES
      TMM1 = 0
      TSS1 = 0
      IF ( THH1 .LT. 0 ) THEN
        THH1 = THH1 + 24
        IFCDA_INI = IFCDA_INI + 1
      ENDIF
      CALL DATE_SUB_DAYS( DYYYY, DMM, DDD, -IFCDA_INI, DYYYY1, DMM1, DDD1 )
    ENDIF

    ! Output date and time
    IINDAT = PACK_YYYYMMDD( DYYYY1, DMM1, DDD1 )
    ITIME  = PACK_HHMM( THH1, TMM1 )

    ! Set time into the grib handle
    PP_METADATA_SET( METADATA,  'dataDate', IINDAT )
    PP_METADATA_SET( METADATA,  'time',     ITIME  )

  END ASSOCIATE

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE TIME_PRESET_GRIBX_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE TIME_PRESET_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
