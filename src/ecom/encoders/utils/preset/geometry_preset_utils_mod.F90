#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'geometry_preset_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GEOMETRY_PRESET_UTILS_MOD'
MODULE GEOMETRY_PRESET_UTILS_MOD

IMPLICIT NONE

! Default visibility
PRIVATE

! Whitelist of public symbols
PUBLIC :: GEOMETRY_PRESET_INIT
PUBLIC :: GEOMETRY_PRESET_FREE

PUBLIC :: GG_GEOMETRY_PRESET_GRIB2_WAM
PUBLIC :: GG_GEOMETRY_PRESET_GRIBX_ATM
PUBLIC :: SH_GEOMETRY_PRESET_GRIBX_ATM
PUBLIC :: VERTICAL_LEVELS_PRESET_GRIBX_ATM

CONTAINS


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GEOMETRY_PRESET_INIT'
SUBROUTINE GEOMETRY_PRESET_INIT( CFG, MODEL_PARAMS )

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

END SUBROUTINE GEOMETRY_PRESET_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GEOMETRY_PRESET_FREE'
SUBROUTINE GEOMETRY_PRESET_FREE( )

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

END SUBROUTINE GEOMETRY_PRESET_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GG_GEOMETRY_PRESET_GRIB2_WAM'
SUBROUTINE GG_GEOMETRY_PRESET_GRIB2_WAM( MODEL_PARAMS, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,       ONLY: JPIB_K
  USE :: OM_CORE_MOD,       ONLY: JPIB_K
  USE :: OM_CORE_MOD,       ONLY: JPRD_K
  USE :: OM_CORE_MOD,       ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,       ONLY: MODEL_PAR_T
  USE :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables
  INTEGER(KIND=JPIB_K) :: NY
  INTEGER(KIND=JPIB_K) :: JC
  INTEGER(KIND=JPIB_K) :: JSN
  INTEGER(KIND=JPIB_K) :: KST
  REAL(KIND=JPRD_K), ALLOCATABLE, DIMENSION(:) :: PL

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Grid type
  IF ( MODEL_PARAMS%WAM_%IQGAUSS .EQ. 1 ) THEN
    PP_METADATA_SET( METADATA,  'gridType','reduced_gg')
  ELSE
    IF ( MODEL_PARAMS%WAM_%IRGG .EQ. 0) THEN
      PP_METADATA_SET( METADATA,  'gridType','regular_ll' )
    ELSE
      PP_METADATA_SET( METADATA,  'gridType','reduced_ll' )
    ENDIF
  ENDIF

  ! Number of points along a meridian
  IF ( MODEL_PARAMS%WAM_%CLDOMAIN .EQ. 'g' .AND.   &
&      MODEL_PARAMS%WAM_%IQGAUSS  .NE. 1   ) THEN
    NY = NINT(180.0_JPRD_K/MODEL_PARAMS%WAM_%XDELLA) + 1
  ELSE
    NY = MODEL_PARAMS%WAM_%NGY
  ENDIF
  PP_METADATA_SET( METADATA,  'Nj', NY )
  IF ( MODEL_PARAMS%WAM_%IQGAUSS .EQ. 1 ) THEN
    PP_METADATA_SET( METADATA,  'N', NY/2 )
  ENDIF

  ! Number of points per latitude
  IF ( MODEL_PARAMS%WAM_%IRGG .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'Ni', MODEL_PARAMS%WAM_%NGX )
  ELSE
    ALLOCATE(PL(NY))
    PL(:)=0
    IF ( MODEL_PARAMS%WAM_%CLDOMAIN .EQ. 'g' .AND. &
&        MODEL_PARAMS%WAM_%IQGAUSS  .NE. 1 ) THEN
      KST = NINT((90.0_JPRD_K -  MODEL_PARAMS%WAM_%AMONOP ) /  MODEL_PARAMS%WAM_%XDELLA)
    ELSE
      KST = 0
    ENDIF
    DO JC = 1,  MODEL_PARAMS%WAM_%NGY
      JSN =  MODEL_PARAMS%WAM_%NGY - JC + 1
      PL(JC+KST) =  MODEL_PARAMS%WAM_%NLONRGG(JSN)
    ENDDO
    PP_METADATA_SET( METADATA,  'pl', PL )
    DEALLOCATE(PL)
  ENDIF

  ! Resolution component flags
  IF ( MODEL_PARAMS%WAM_%IQGAUSS .EQ. 1 ) THEN
    PP_METADATA_SET( METADATA,  'resolutionAndComponentFlags', 0 )
  ELSE
    PP_METADATA_SET( METADATA,  'resolutionAndComponentFlags', 128 )
  ENDIF


  ! Latitude of the first grid point
  IF ( MODEL_PARAMS%WAM_%CLDOMAIN .EQ. 'g' .AND.  &
&      MODEL_PARAMS%WAM_%IQGAUSS  .NE. 1 ) THEN
    PP_METADATA_SET( METADATA,  'latitudeOfFirstGridPointInDegrees', 90. )
  ELSE
    PP_METADATA_SET( METADATA,  'latitudeOfFirstGridPointInDegrees', MODEL_PARAMS%WAM_%AMONOP )
  ENDIF

  ! Longitude of the origin (west - )
  PP_METADATA_SET( METADATA,   'longitudeOfFirstGridPointInDegrees', MODEL_PARAMS%WAM_%AMOWEP )

  ! Latitude of last grid point
  IF ( MODEL_PARAMS%WAM_%CLDOMAIN .EQ. 'g' .AND.  &
&      MODEL_PARAMS%WAM_%IQGAUSS  .NE. 1 ) THEN
    PP_METADATA_SET( METADATA,  'latitudeOfLastGridPointInDegrees', -90. )
  ELSE
    PP_METADATA_SET( METADATA,  'latitudeOfLastGridPointInDegrees',  MODEL_PARAMS%WAM_%AMOSOP )
  ENDIF

  ! Longitude of extreme grid points (west)
  PP_METADATA_SET( METADATA,  'longitudeOfLastGridPointInDegrees', MODEL_PARAMS%WAM_%AMOEAP )

  ! Longitude increment
  IF ( MODEL_PARAMS%WAM_%IRGG .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'iDirectionIncrementInDegrees', MODEL_PARAMS%WAM_%XDELLO )
  ENDIF

  ! Latitude increment
  IF ( MODEL_PARAMS%WAM_%IQGAUSS .NE. 1 ) THEN
    PP_METADATA_SET( METADATA,  'jDirectionIncrementInDegrees', MODEL_PARAMS%WAM_%XDELLA )
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE GG_GEOMETRY_PRESET_GRIB2_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Sets the geometry metadata (section 3 in GRIB2) for gridded fields.
!>
!> This function sets the geometry metadata for gridded fields of a simulation into the provided metadata object
!> using information retrieved from the specified data structure (YDIOS).
!>
!> @param [inout] METADATA Metadata object where the geometry metadata will be set.
!> @param [in]    YDIOS    Data structure used to retrieve the necessary information for setting the metadata.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GG_GEOMETRY_PRESET_GRIBX_ATM'
SUBROUTINE GG_GEOMETRY_PRESET_GRIBX_ATM( MODEL_PARAMS, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,       ONLY: JPIB_K
  USE :: OM_CORE_MOD,       ONLY: JPRD_K
  USE :: OM_CORE_MOD,       ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,       ONLY: MODEL_PAR_T
  USE :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables
  REAL(KIND=JPRD_K) :: LAT_FIRST_GP_DEG
  REAL(KIND=JPRD_K) :: LAT_LAST_GP_DEG
  REAL(KIND=JPRD_K) :: LON_LAST_GP_DEG

  ! TODO: Verify this: in the original implementation NDLON was used instead of ILONS
  REAL(KIND=JPRD_K) :: IDIR_INC

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ASSOCIATE ( YPI => MODEL_PARAMS%GEO_ )

    LAT_FIRST_GP_DEG=180._JPRD_K/(2.0_JPRD_K*ASIN(1.0_JPRD_K))*YPI%ZNLAT
    LAT_LAST_GP_DEG=180._JPRD_K/(2.0_JPRD_K*ASIN(1.0_JPRD_K))*YPI%ZSLAT
    LON_LAST_GP_DEG=360._JPRD_K-360._JPRD_K/REAL(YPI%ILONS,JPRD_K)
    IDIR_INC=360.0_JPRD_K/REAL(YPI%ILONS,JPRD_K)

    PP_METADATA_SET( METADATA,  'truncateDegrees',1)
    PP_METADATA_SET( METADATA,  'numberOfPointsAlongAMeridian',YPI%ILATS)

    PP_METADATA_SET( METADATA,  'latitudeOfFirstGridPointInDegrees',LAT_FIRST_GP_DEG)
    PP_METADATA_SET( METADATA,  'longitudeOfFirstGridPointInDegrees',0)

    PP_METADATA_SET( METADATA,  'latitudeOfLastGridPointInDegrees',LAT_LAST_GP_DEG)
    PP_METADATA_SET( METADATA,  'longitudeOfLastGridPointInDegrees',LON_LAST_GP_DEG)

    PP_METADATA_SET( METADATA,  'numberOfParallelsBetweenAPoleAndTheEquator',YPI%IDGNH)

    !     NHTYP  : 0 = regular grid
    !            : 2 = number of points read on namelist namrgri
    IF( YPI%NHTYP .GT. 0 ) THEN
      PP_METADATA_SET( METADATA,  'gridType','reduced_gg')
      PP_METADATA_SET( METADATA,  'pl',YPI%ILOENG(1:YPI%ILATS))
    ELSE
      PP_METADATA_SET( METADATA,  'gridType','regular_gg')
      PP_METADATA_SET( METADATA,  'numberOfPointsAlongAParallel',YPI%ILONS)
      PP_METADATA_SET( METADATA,  'iDirectionIncrementInDegrees',IDIR_INC)
    ENDIF

  END ASSOCIATE

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE GG_GEOMETRY_PRESET_GRIBX_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Sets the geometry metadata (section 3 in GRIB2) for spherical harmonics fields.
!>
!> This function sets the geometry metadata for spherical harmonics  fields of a simulation into the provided metadata object
!> using information retrieved from the specified data structure (YDIOS).
!>
!> @param [inout] METADATA Metadata object where the geometry metadata will be set.
!> @param [in]    YDIOS    Data structure used to retrieve the necessary information for setting the metadata.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SH_GEOMETRY_PRESET_GRIBX_ATM'
SUBROUTINE SH_GEOMETRY_PRESET_GRIBX_ATM( MODEL_PARAMS, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,       ONLY: JPIB_K
  USE :: OM_CORE_MOD,       ONLY: JPRB_K
  USE :: OM_CORE_MOD,       ONLY: JPRD_K
  USE :: OM_CORE_MOD,       ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,       ONLY: MODEL_PAR_T
  USE :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables
  REAL(KIND=JPRD_K) :: LAT_STRET_DEG
  REAL(KIND=JPRD_K) :: LON_STRET_DEG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ASSOCIATE ( YPI => MODEL_PARAMS%GEO_ )

    ! NSTTYP: 1 = POLE OF STRETCHING, POLE OF THE COLLOCATION GRID
    !             AT THE NORTHERN POLE OF THE REAL EARTH.
    !         2 = THE POLE OF STRETCHING IS ANYWHERE ON THE REAL EARTH
    !             AND ON THE EQUATOR OF THE COLLOCATION GRID ON THE MERIDIAN PI.
    !             THE EQUATOR OF THE COLLOCATION GRID IS TANGENT
    !             TO A PARALLEL OF THE EARTH.

    ! RSTRET: STRETCHING FACTOR
    ! RMUCEN: MU OF THE POLE OF STRETCHING
    ! RLOCEN: LONGITUDE OF THE POLE OF STRETCHING
    LAT_STRET_DEG = 180._JPRD_K/(2.0_JPRD_K*ASIN(1.0_JPRD_K))*ASIN(REAL(YPI%RMUCEN,JPRD_K))
    LON_STRET_DEG = 180._JPRD_K/(2.0_JPRD_K*ASIN(1.0_JPRD_K))*REAL(YPI%RLOCEN,JPRD_K)

    IF( YPI%NSTTYP .GE. 2 ) THEN

      PP_METADATA_SET( METADATA,  'gridType','stretched_rotated_sh')
      PP_METADATA_SET( METADATA,  'latitudeOfStretchingPoleInDegrees', LAT_STRET_DEG )
      PP_METADATA_SET( METADATA,  'longitudeOfStretchingPoleInDegrees', LON_STRET_DEG )
      PP_METADATA_SET( METADATA,  'stretchingFactor',YPI%RSTRET)

    ELSEIF( ABS(YPI%RSTRET-1.0_JPRB_K) .GE. 1.E-14_JPRB_K ) THEN

      PP_METADATA_SET( METADATA,  'gridType', 'stretched_sh' )
      PP_METADATA_SET( METADATA,  'stretchingFactor', YPI%RSTRET )

    ELSE

      PP_METADATA_SET( METADATA,  'gridType','sh')

    ENDIF

    PP_METADATA_SET( METADATA,  'pentagonalResolutionParameterJ',YPI%ISMAX)
    PP_METADATA_SET( METADATA,  'pentagonalResolutionParameterK',YPI%ISMAX)
    PP_METADATA_SET( METADATA,  'pentagonalResolutionParameterM',YPI%ISMAX)

  END ASSOCIATE

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE SH_GEOMETRY_PRESET_GRIBX_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Sets the vertical levels metadata for fields on model levels.
!>
!> This function sets the vertical levels metadata for fields on model levels of a simulation into the provided metadata object
!> using information retrieved from the specified data structure (YDIOS).
!>
!> @param [inout] METADATA Metadata object where the geometry metadata will be set.
!> @param [in]    YDIOS    Data structure used to retrieve the necessary information for setting the metadata.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'VERTICAL_LEVELS_PRESET_GRIBX_ATM'
SUBROUTINE VERTICAL_LEVELS_PRESET_GRIBX_ATM( MODEL_PARAMS, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,       ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,       ONLY: MODEL_PAR_T
  USE :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  PP_METADATA_SET( METADATA,  'pv', MODEL_PARAMS%GEO_%ZVERT(1:2*(MODEL_PARAMS%GEO_%IFLEV+1)))

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE VERTICAL_LEVELS_PRESET_GRIBX_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE GEOMETRY_PRESET_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
