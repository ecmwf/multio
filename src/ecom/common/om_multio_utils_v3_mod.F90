!> @file
!>
!> @brief Module containing utilities for calling MultIO.
!>
!> This module provides utilities wrap the calls to MultIO
!>
!> @author Philipp Geier (ECMWF)
!> @author Mirco Valentini
!> @date   January 31, 2024
!>


! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'om_multio_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'OM_MULTIO_UTILS_MOD'
MODULE OM_MULTIO_UTILS_MOD

IMPLICIT NONE

! Default visibility
PRIVATE

! Whitelist of public symbols
PUBLIC :: MULTIO_NEW
PUBLIC :: MULTIO_DELETE
PUBLIC :: MULTIO_WRITE_PARAMETRIZATION
PUBLIC :: MULTIO_WRITE_BINARY_GRIB
PUBLIC :: MULTIO_WRITE_VALUES_DP
PUBLIC :: MULTIO_WRITE_VALUES_SP
PUBLIC :: MULTIO_INJECT_PARAMETERS
PUBLIC :: MULTIO_FLUSH_FIRST_STEP
PUBLIC :: MULTIO_FLUSH
PUBLIC :: MULTIO_FLUSH_AND_TRIGGER_RESTART
PUBLIC :: MULTIO_FLUSH_LAST_STEP
PUBLIC :: MULTIO_FLUSH_END_OF_SIMULATION
PUBLIC :: MULTIO_NOTIFY_STEP


CONTAINS


!>
!> @brief initialise a new MultIO handle.
!>
!> This function is used initialise a new multio handle.
!>  - read environment variable [\"MULTIO_IFSIO_CONFIG_FILE\"|\"MULTIO_PLANS_FILE\"]
!>    where the name of the configuration file is defined
!>  - create a new multio_configuration object, used to wrap all the
!>    information needed to create a MultIO handle
!>  - set the failure handler function
!>  - create the MultIO handle
!>
!> @param [inout] mio_handle A MultIO handle
!>
!> @see MULTIO_DELETE
!> @see MULTIO_ERROR_HANDLER
!> @see MULTIO_GET_CONFIG_FILE_NAME
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_NEW'
SUBROUTINE MULTIO_NEW( MIO_HANDLE, CONFIG_FILE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K

  ! Symbols imported from other modules within the project.
  USE :: MULTIO_API,  ONLY: MULTIO_HANDLE
  USE :: MULTIO_API,  ONLY: MULTIO_CONFIGURATION
  USE :: MULTIO_API,  ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API,  ONLY: MULTIO_ERROR_STRING
  USE :: MULTIO_API,  ONLY: FAILURE_HANDLER_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE), INTENT(INOUT) :: MIO_HANDLE
  CHARACTER(LEN=*),    INTENT(IN)    :: CONFIG_FILE

  ! Local variabels
  TYPE(MULTIO_CONFIGURATION) :: MIOCC
  INTEGER(KIND=JPIM_K) :: ERR
  INTEGER(KIND=INT64)  :: ERR_CONTEXT
  PROCEDURE(FAILURE_HANDLER_T), POINTER :: PF

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Create the MultIO configuration object
  ERR = MIOCC%NEW( TRIM(ADJUSTL(CONFIG_FILE)) )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

  ! Setup the failure handler
  PF => MULTIO_ERROR_HANDLER
  ERR = MIOCC%SET_FAILURE_HANDLER( PF, ERR_CONTEXT )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  3 )

  ! Create the MultIO handle
  ERR = MIO_HANDLE%NEW( MIOCC )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  4 )

  ! Destroy the MultIO configuration object
  ERR = MIOCC%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  5 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create MultIO config from file: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (2)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create MultIO config: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (3)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create MultIO failure handler: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (4)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create MultIO handle: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (5)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to destroy MultIO handle: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_NEW
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief finalise a new MultIO handle.
!>
!> This function is used finalise a multio handle.
!>
!> @param [inout] mio_handle A MultIO handle
!>
!> @see MULTIO_NEW
!> @see MULTIO_ERROR_HANDLER
!> @see MULTIO_GET_CONFIG_FILE_NAME
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_DELETE'
SUBROUTINE MULTIO_DELETE( MIO_HANDLE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: MULTIO_API,  ONLY: MULTIO_HANDLE
  USE :: MULTIO_API,  ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API,  ONLY: MULTIO_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE), INTENT(INOUT) :: MIO_HANDLE

  ! Local variables
  INTEGER(KIND=JPIM_K) :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Delete the multio handle
  ERR = MIO_HANDLE%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to finalise MultIO handle: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_DELETE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief send parametrization to multio.
!>
!> @param [inout] mio_handle        A MultIO handle
!> @param [in]    model_params      The model params - constant information for the whole run
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_WRITE_PARAMETRIZATION'
SUBROUTINE MULTIO_WRITE_PARAMETRIZATION( MIO_HANDLE, MODEL_PARAMS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIM_K

  ! Symbols imported from other libraries
  USE :: GRIB_API,   ONLY: GRIB_SUCCESS
  USE :: GRIB_API,   ONLY: GRIB_GET_MESSAGE_SIZE
  USE :: GRIB_API,   ONLY: GRIB_COPY_MESSAGE
  USE :: GRIB_API,   ONLY: GRIB_GET_ERROR_STRING
  USE :: MULTIO_API, ONLY: MULTIO_HANDLE
  USE :: MULTIO_API, ONLY: MULTIO_METADATA
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE),                         INTENT(INOUT) :: MIO_HANDLE
  TYPE(MODEL_PAR_T),                           INTENT(IN)    :: MODEL_PARAMS

  ! Local variables
  TYPE(MULTIO_METADATA) :: MIOMD
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

#if 0
  ERR = MIOMD%NEW( MIO_HANDLE )
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )


  ERR = MIOMD%SET('generatingProcessIdentifier', MODEL_PARAMS%SIM_%NCYCLE )
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

  ERR = MIOMD%SET('iterationNumber', MODEL_PARAMS%SIM_%NJITER)
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
  ERR = MIOMD%SET('sensitiveAreaDomain', MODEL_PARAMS%SIM_%NJDOMAI)
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
  ERR = MIOMD%SET('diagnosticNumber', MODEL_PARAMS%SIM_%NJDIAG)
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
  ! TODO look at mars_preset_utils_mod.F90 to see how this is used for ccccIdentifiers and consensusCount
  ! ERR = MIOMD%SET('', MODEL_PARAMS%SIM_%NDWD)
  ! ERR = MIOMD%SET('', MODEL_PARAMS%SIM_%NMFR)
  ! ERR = MIOMD%SET('', MODEL_PARAMS%SIM_%NNCEP)
  ! ERR = MIOMD%SET('', MODEL_PARAMS%SIM_%NUKM)
  ERR = MIOMD%SET('perturbationNumber', MODEL_PARAMS%SIM_%NENSFNB)
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
  ERR = MIOMD%SET('numberOfForecastsInEnsemble', MODEL_PARAMS%SIM_%NTOTENS)
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
  ERR = MIOMD%SET('systemNumber', MODEL_PARAMS%SIM_%NSYSTEM)
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
  ERR = MIOMD%SET('methodNumber', MODEL_PARAMS%SIM_%NMETHOD)
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
  ERR = MIOMD%SET('referenceDate', MODEL_PARAMS%SIM_%NREFERENCE)
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
  ERR = MIOMD%SET('anoffset', MODEL_PARAMS%SIM_%NWINOFF)
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
  ERR = MIOMD%SET('expver', MODEL_PARAMS%SIM_%CNMEXP)
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
  ERR = MIOMD%SET('class', MODEL_PARAMS%SIM_%CFCLASS)
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
  ERR = MIOMD%SET('type', MODEL_PARAMS%SIM_%CTYPE)
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
  ERR = MIOMD%SET('stream', MODEL_PARAMS%SIM_%NSTREAM)
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

  ! TODO WRITE as array
  ERR = MIO_HANDLE%WRITE_PARAMETRIZATION('pv', MODEL_PARAMS%GEO_%ZVERT(1:2*(MODEL_PARAMS%GEO_%IFLEV+1)))
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  2 )



  ASSOCIATE ( YPI => MODEL_PARAMS%GEO_ )
    BLOCK
    REAL(KIND=JPRD_K) :: LAT_FIRST_GP_DEG
    REAL(KIND=JPRD_K) :: LAT_LAST_GP_DEG
    REAL(KIND=JPRD_K) :: LON_LAST_GP_DEG
    ! TODO: Verify this: in the original implementation NDLON was used instead of ILONS
    REAL(KIND=JPRD_K) :: IDIR_INC


    LAT_FIRST_GP_DEG=180._JPRD_K/(2.0_JPRD_K*ASIN(1.0_JPRD_K))*YPI%ZNLAT
    LAT_LAST_GP_DEG=180._JPRD_K/(2.0_JPRD_K*ASIN(1.0_JPRD_K))*YPI%ZSLAT
    LON_LAST_GP_DEG=360._JPRD_K-360._JPRD_K/REAL(YPI%ILONS,JPRD_K)
    IDIR_INC=360.0_JPRD_K/REAL(YPI%ILONS,JPRD_K)

    ERR = MIOMD%SET('truncateDegrees',1)
    PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
    ERR = MIOMD%SET('numberOfPointsAlongAMeridian',YPI%ILATS)
    PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

    ERR = MIOMD%SET('latitudeOfFirstGridPointInDegrees',LAT_FIRST_GP_DEG)
    PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
    ERR = MIOMD%SET('longitudeOfFirstGridPointInDegrees',0)
    PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

    ERR = MIOMD%SET('latitudeOfLastGridPointInDegrees',LAT_LAST_GP_DEG)
    PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
    ERR = MIOMD%SET('longitudeOfLastGridPointInDegrees',LON_LAST_GP_DEG)
    PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

    ERR = MIOMD%SET('numberOfParallelsBetweenAPoleAndTheEquator',YPI%IDGNH)
    PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

    !     NHTYP  : 0 = regular grid
    !            : 2 = number of points read on namelist namrgri
    IF( YPI%NHTYP .GT. 0 ) THEN
      ERR = MIOMD%SET('gridType','reduced_gg')
      PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
      ERR = MIO_HANDLE%WRITE_PARAMETRIZATION('pl', YPI%ILOENG(1:YPI%ILATS))
      PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  2 )
    ELSE
      ERR = MIOMD%SET('gridType','regular_gg')
      PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
      ERR = MIOMD%SET('numberOfPointsAlongAParallel',YPI%ILONS)
      PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
      ERR = MIOMD%SET('iDirectionIncrementInDegrees',IDIR_INC)
      PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
    ENDIF

    END BLOCK
  END ASSOCIATE



  ASSOCIATE ( YPI => MODEL_PARAMS%GEO_ )
    BLOCK
        REAL(KIND=JPRD_K) :: LAT_STRET_DEG
        REAL(KIND=JPRD_K) :: LON_STRET_DEG

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
          ERR = MIOMD%SET('gridType','stretched_rotated_sh')
          PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
          ERR = MIOMD%SET('latitudeOfStretchingPoleInDegrees', LAT_STRET_DEG )
          PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
          ERR = MIOMD%SET('longitudeOfStretchingPoleInDegrees', LON_STRET_DEG )
          PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
          ERR = MIOMD%SET('stretchingFactor',YPI%RSTRET)
          PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
        ELSEIF( ABS(YPI%RSTRET-1.0_JPRB_K) .GE. 1.E-14_JPRB_K ) THEN
          ERR = MIOMD%SET('gridType', 'stretched_sh' )
          PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
          ERR = MIOMD%SET('stretchingFactor', YPI%RSTRET )
          PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
        ELSE

          ERR = MIOMD%SET('gridType','sh')
          PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

        ENDIF

        ERR = MIOMD%SET('pentagonalResolutionParameterJ',YPI%ISMAX)
        PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
        ERR = MIOMD%SET('pentagonalResolutionParameterK',YPI%ISMAX)
        PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
        ERR = MIOMD%SET('pentagonalResolutionParameterM',YPI%ISMAX)
        PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
    END BLOCK
  END ASSOCIATE



  BLOCK
    ! Local variables
    INTEGER(KIND=JPIB_K) :: KK
    INTEGER(KIND=JPIB_K) :: MM
    INTEGER(KIND=JPIB_K) :: IDIRSCALING
    INTEGER(KIND=JPIB_K) :: IFRESCALING

    IDIRSCALING = 2
    IFRESCALING = 6
    IF ( .NOT.ALLOCATED(SCTH) ) THEN
      ALLOCATE(SCTH(MODEL_PARAMS%WAM_%NANG))
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(MODEL_PARAMS%WAM_%TH), 1 )
      DO KK=1,MODEL_PARAMS%WAM_%NANG
         SCTH(KK)=NINT(MODEL_PARAMS%WAM_%TH(KK)*10**IDIRSCALING*DEG)
      ENDDO
    ENDIF
    IF ( .NOT.ALLOCATED(SCFR) ) THEN
      ALLOCATE(SCFR(MODEL_PARAMS%WAM_%NFRE_RED))
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(MODEL_PARAMS%WAM_%TH), 2 )
      DO MM=1,MODEL_PARAMS%WAM_%NFRE_RED
        SCFR(MM)=NINT(MODEL_PARAMS%WAM_%FR(MM)*10**IFRESCALING)
      ENDDO
    ENDIF

    ERR = MIOMD%SET('numberOfWaveDirections', MODEL_PARAMS%WAM_%NANG )
    PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
    ERR = MIOMD%SET('scaleFactorOfWaveDirections', IDIRSCALING )
    PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
    ERR = MIOMD%SET('numberOfWaveFrequencies', MODEL_PARAMS%WAM_%NFRE_RED )
    PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )
    ERR = MIOMD%SET('scaleFactorOfWaveFrequencies', IFRESCALING )
    PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

    ERR = MIO_HANDLE%WRITE_PARAMETRIZATION('scaledValuesOfWaveDirections', SCTH)
    PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  3 )
    ERR = MIO_HANDLE%WRITE_PARAMETRIZATION('scaledValuesOfWaveFrequencies', SCFR)
    PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  3 )

    IF ( ALLOCATED(SCTH) ) DEALLOCATE(SCTH)
    IF ( ALLOCATED(SCFR) ) DEALLOCATE(SCFR)
  END BLOCK


  BLOCK
      INTEGER(KIND=JPIB_K), DIMENSION(SIZE(MODEL_PARAMS%GEO_%NSFLEVS))  :: NSFLEVS
      NSFLEVS = MODEL_PARAMS%GEO_%NSFLEVS(:,:)

      ERR = MIO_HANDLE%WRITE_PARAMETRIZATION('paramIdTopBottomMap', NSFLEVS)
      PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  3 )
  END BLOCK

#endif


  ERR = MIO_HANDLE%WRITE_PARAMETRIZATION(MIOMD)
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS,  2 )

  ERR = MIOMD%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  4 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'MULTIO_WRITE_PARAMETRIZATION: Unable to set a metadata key: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'MULTIO_WRITE_PARAMETRIZATION: Unable to set a metadata key' )
      ENDIF
    CASE (2)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'MULTIO_WRITE_PARAMETRIZATION: Unable to write parametrization: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'MULTIO_WRITE_PARAMETRIZATION: Unable to write parametrization' )
      ENDIF
    CASE (3)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'MULTIO_WRITE_PARAMETRIZATION: Unable to write parametrization array: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'MULTIO_WRITE_PARAMETRIZATION: Unable to write parametrization array' )
      ENDIF
    CASE (4)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'MULTIO_WRITE_PARAMETRIZATION: Unable to delete metadata: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'MULTIO_WRITE_PARAMETRIZATION: Unable to delete metadata' )
      ENDIF
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_WRITE_PARAMETRIZATION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



!>
!> @brief send a encoded buffer through multIO.
!>
!> @param [inout] mio_handle A MultIO handle
!> @param [in]    istep      The current step to be notified
!>
!> @see MULTIO_FLUSH_AND_TRIGGER_RESTART
!> @see MULTIO_FLUSH_LAST_STEP
!> @see MULTIO_FLUSH_END_OF_SIMULATION
!> @see MULTIO_NOTIFY_STEP
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_WRITE_BINARY_GRIB'
SUBROUTINE MULTIO_WRITE_BINARY_GRIB( MIO_HANDLE, BUFFER, HANDLE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIM_K

  ! Symbols imported from other libraries
  USE :: GRIB_API,   ONLY: GRIB_SUCCESS
  USE :: GRIB_API,   ONLY: GRIB_GET_MESSAGE_SIZE
  USE :: GRIB_API,   ONLY: GRIB_COPY_MESSAGE
  USE :: GRIB_API,   ONLY: GRIB_GET_ERROR_STRING
  USE :: MULTIO_API, ONLY: MULTIO_HANDLE
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE),                         INTENT(INOUT) :: MIO_HANDLE
  CHARACTER(LEN=1), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: BUFFER
  INTEGER(KIND=JPIM_K),                        INTENT(IN)    :: HANDLE

  ! Local variables
  INTEGER(KIND=JPIM_K) :: DATA_LENGTH
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIM_K) :: KRET
  INTEGER(KIND=JPIM_K) :: ERR
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()


  ! Get encoded grib message size
  KRET = GRIB_SUCCESS
  CALL GRIB_GET_MESSAGE_SIZE( HANDLE, DATA_LENGTH, STATUS=KRET)
  PP_DEBUG_CRITICAL_COND_THROW(KRET.NE.GRIB_SUCCESS, 1)
  PP_DEBUG_CRITICAL_COND_THROW(DATA_LENGTH.LE.0, 2)

  ! Maange buffer allocation
  IF (ALLOCATED(BUFFER) .AND. (SIZE(BUFFER) .LT. DATA_LENGTH)) THEN
    DEALLOCATE(BUFFER, STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_DEVELOP_COND_THROW(STAT.NE.0, 3)
  ENDIF

  IF (.NOT.ALLOCATED(BUFFER)) THEN
    STAT = 0
    ALLOCATE(BUFFER(DATA_LENGTH), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_DEVELOP_COND_THROW(STAT.NE.0, 4)
  ENDIF

  ! Extract encoded message
  KRET = GRIB_SUCCESS
  CALL GRIB_COPY_MESSAGE(HANDLE, BUFFER, STATUS=KRET)
  PP_DEBUG_CRITICAL_COND_THROW(KRET.NE.GRIB_SUCCESS, 5)

  ! Write encoded message to multio
  ERR = MIO_HANDLE%WRITE_GRIB_ENCODED( BUFFER )
  PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, 6)


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      CALL GRIB_GET_ERROR_STRING(KRET, GRIB_ERROR)
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to read the buffer size: ', KRET, GRIB_ERROR )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Buffer size must be greater than 0' )
    CASE (3)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate buffer: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate buffer' )
      ENDIF
    CASE (4)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate buffer: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate buffer' )
      ENDIF
    CASE (5)
      CALL GRIB_GET_ERROR_STRING(KRET, GRIB_ERROR)
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to copy the message to the buffer: ', KRET, GRIB_ERROR )
    CASE (6)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write encoded grib trough multio: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write encoded grib trough multio' )
      ENDIF
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_WRITE_BINARY_GRIB
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief send a encoded buffer through multIO.
!>
!> @param [inout] mio_handle A MultIO handle
!> @param [in]    istep      The current step to be notified
!>
!> @see MULTIO_FLUSH_AND_TRIGGER_RESTART
!> @see MULTIO_FLUSH_LAST_STEP
!> @see MULTIO_FLUSH_END_OF_SIMULATION
!> @see MULTIO_NOTIFY_STEP
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_WRITE_VALUES_DP'
SUBROUTINE MULTIO_WRITE_VALUES_DP( MIOH, MIOMD, VDP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPRD_K
  USE :: OM_CORE_MOD, ONLY: JPIM_K

  ! Symbols imported from other libraries
  USE :: MULTIO_API, ONLY: MULTIO_HANDLE
  USE :: MULTIO_API, ONLY: MULTIO_METADATA
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE),             INTENT(INOUT) :: MIOH
  TYPE(MULTIO_METADATA),           INTENT(INOUT) :: MIOMD
  REAL(KIND=JPRD_K), DIMENSION(:), INTENT(IN)    :: VDP

  ! Local variables
  INTEGER(KIND=JPIM_K) :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Write size
  ERR = MIOMD%SET( 'globalSize', SIZE(VDP) )
  PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, 1)

  ! Write to multio
  ERR = MIOH%WRITE_FIELD( MIOMD, VDP )
  PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, 2)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write values size into multio metadata: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write values size into multio metadata' )
      ENDIF
    CASE (2)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write values into multio handle: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write values into multio handle' )
      ENDIF
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_WRITE_VALUES_DP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief send a encoded buffer through multIO.
!>
!> @param [inout] mio_handle A MultIO handle
!> @param [in]    istep      The current step to be notified
!>
!> @see MULTIO_FLUSH_AND_TRIGGER_RESTART
!> @see MULTIO_FLUSH_LAST_STEP
!> @see MULTIO_FLUSH_END_OF_SIMULATION
!> @see MULTIO_NOTIFY_STEP
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_WRITE_VALUES_SP'
SUBROUTINE MULTIO_WRITE_VALUES_SP( MIOH, MIOMD, VSP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPRM_K
  USE :: OM_CORE_MOD, ONLY: JPIM_K

  ! Symbols imported from other libraries
  USE :: MULTIO_API, ONLY: MULTIO_HANDLE
  USE :: MULTIO_API, ONLY: MULTIO_METADATA
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE),             INTENT(INOUT) :: MIOH
  TYPE(MULTIO_METADATA),           INTENT(INOUT) :: MIOMD
  REAL(KIND=JPRM_K), DIMENSION(:), INTENT(IN)    :: VSP

  ! Local variables
  INTEGER(KIND=JPIM_K) :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Write size
  ERR = MIOMD%SET( 'globalSize', SIZE(VSP) )
  PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, 1)

  ! Write to multio
  ERR = MIOH%WRITE_FIELD( MIOMD, VSP )
  PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, 2)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write values size into multio metadata: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write values size into multio metadata' )
      ENDIF
    CASE (2)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write values into multio handle: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write values into multio handle' )
      ENDIF
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_WRITE_VALUES_SP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief send a encoded buffer through multIO.
!>
!> @param [inout] mio_handle A MultIO handle
!> @param [in]    istep      The current step to be notified
!>
!> @see MULTIO_FLUSH_AND_TRIGGER_RESTART
!> @see MULTIO_FLUSH_LAST_STEP
!> @see MULTIO_FLUSH_END_OF_SIMULATION
!> @see MULTIO_NOTIFY_STEP
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_INJECT_PARAMETERS'
SUBROUTINE MULTIO_INJECT_PARAMETERS( MODEL_PARAMS, MIOMD)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD, ONLY: JPRM_K
  USE :: OM_CORE_MOD, ONLY: JPIM_K

  ! Symbols imported from other libraries
  USE :: MULTIO_API, ONLY: MULTIO_METADATA
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),     INTENT(IN)    :: MODEL_PARAMS
  TYPE(MULTIO_METADATA), INTENT(INOUT) :: MIOMD

  ! Local variables
  INTEGER(KIND=JPIM_K)  :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! TODO: If needed here we can inject more parameters in the metadata of each message

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MULTIO_INJECT_PARAMETERS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





!>
!> @brief send a flush to MultiIO using the provided handle to notify
!>        multIO that the simulation started.
!>
!> This function is used to send a flush message to the specified MultIO
!> handle. The flush message is used to notify MultIO that the simulation
!> started.
!>
!> @param [inout] mio_handle A MultIO handle
!>
!> @see MULTIO_FLUSH_AND_TRIGGER_RESTART
!> @see MULTIO_FLUSH_LAST_STEP
!> @see MULTIO_FLUSH_END_OF_SIMULATION
!> @see MULTIO_NOTIFY_STEP
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_FLUSH_FIRST_STEP'
SUBROUTINE MULTIO_FLUSH_FIRST_STEP( MIO_HANDLE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: MULTIO_API,  ONLY: MULTIO_HANDLE
  USE :: MULTIO_API,  ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API,  ONLY: MULTIO_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE), INTENT(INOUT) :: MIO_HANDLE

  ! Local variables
  TYPE(MULTIO_METADATA) :: MIOMD
  INTEGER(KIND=JPIM_K)  :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ERR = MIOMD%NEW( MIO_HANDLE )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

  ERR = MIOMD%SET( "flushKind", 0 )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  2 )

  ERR = MIOMD%SET( "step", -1 )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  3 )

  ERR = MIO_HANDLE%FLUSH( MIOMD )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  4 )

  ERR = MIOMD%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  5 )


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create a new MultIO metadata object' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set \"flushKind\" in the MultIO metadata' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set \"step\" in the MultIO metadata' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to call the flush' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to delete MultIO metadata object' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_FLUSH_FIRST_STEP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





!>
!> @brief send a flush to MultiIO using the provided handle.
!>
!> This function is used to send a flush message to the specified MultIO
!> handle.
!>
!> @param [inout] mio_handle A MultIO handle
!> @param [in]    istep      The current step to be notified
!>
!> @see MULTIO_FLUSH_AND_TRIGGER_RESTART
!> @see MULTIO_FLUSH_LAST_STEP
!> @see MULTIO_FLUSH_END_OF_SIMULATION
!> @see MULTIO_NOTIFY_STEP
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_FLUSH'
SUBROUTINE MULTIO_FLUSH( MIO_HANDLE, ISTEP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: MULTIO_API,  ONLY: MULTIO_HANDLE
  USE :: MULTIO_API,  ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API,  ONLY: MULTIO_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE), INTENT(INOUT) :: MIO_HANDLE
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: ISTEP

  ! Local variables
  TYPE(MULTIO_METADATA) :: MIOMD
  INTEGER(KIND=JPIM_K)  :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ERR = MIOMD%NEW( MIO_HANDLE )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

  ERR = MIOMD%SET( "flushKind", 1 )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  2 )

  ERR = MIOMD%SET( "step", ISTEP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  3 )

  ERR = MIO_HANDLE%FLUSH( MIOMD )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  4 )

  ERR = MIOMD%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  5 )


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create a new MultIO metadata object' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set \"flushKind\" in the MultIO metadata' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set \"step\" in the MultIO metadata' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to call the flush' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to delete MultIO metadata object' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_FLUSH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



!>
!> @brief send a flush to MultiIO using the provided handle.
!>
!> This function is used to send a flush message to the specified MultIO
!> handle. The flush contain also the request to dump a restart to all the
!> actions.
!>
!> @param [inout] mio_handle A MultIO handle
!> @param [in]    istep      The current step to be notified
!>
!> @see MULTIO_FLUSH
!> @see MULTIO_FLUSH_LAST_STEP
!> @see MULTIO_FLUSH_END_OF_SIMULATION
!> @see MULTIO_NOTIFY_STEP
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_FLUSH_AND_TRIGGER_RESTART'
SUBROUTINE MULTIO_FLUSH_AND_TRIGGER_RESTART( MIO_HANDLE, ISTEP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: MULTIO_API,  ONLY: MULTIO_HANDLE
  USE :: MULTIO_API,  ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API,  ONLY: MULTIO_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE),  INTENT(INOUT) :: MIO_HANDLE
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ISTEP

  ! Local variables
  TYPE(MULTIO_METADATA) :: MIOMD
  INTEGER(KIND=JPIM_K)  :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ERR = MIOMD%NEW(MIO_HANDLE)
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

  ERR = MIOMD%SET( "flushKind", 2 )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  2 )

  ERR = MIOMD%SET( "step",  ISTEP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  3 )

  ERR = MIO_HANDLE%FLUSH(MIOMD)
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  4 )

  ERR = MIOMD%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  5 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create a new MultIO metadata object' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set \"flushKind\" in the MultIO metadata' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set \"step\" in the MultIO metadata' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to call the flush' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to delete MultIO metadata object' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_FLUSH_AND_TRIGGER_RESTART
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief send a flush to MultiIO using the provided handle.
!>
!> This function is used to send a flush message to the specified MultIO
!> handle. The flush contain also the information that this is the last
!> step of the simulation (used for example in statistics to dump the
!> last step).
!>
!> @param [inout] mio_handle A MultIO handle
!> @param [in]    istep      The current step to be notified
!>
!> @see MULTIO_FLUSH
!> @see MULTIO_FLUSH_AND_TRIGGER_RESTART
!> @see MULTIO_FLUSH_END_OF_SIMULATION
!> @see MULTIO_NOTIFY_STEP
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_FLUSH_AND_TRIGGER_RESTART'
SUBROUTINE MULTIO_FLUSH_LAST_STEP(MIO_HANDLE, ISTEP)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: MULTIO_API,  ONLY: MULTIO_HANDLE
  USE :: MULTIO_API,  ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API,  ONLY: MULTIO_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE), INTENT(INOUT) :: MIO_HANDLE
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: ISTEP

  ! Local variables
  TYPE(MULTIO_METADATA) :: MIOMD
  INTEGER(KIND=JPIM_K)  :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ERR = MIOMD%NEW( MIO_HANDLE )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

  ERR = MIOMD%SET( "flushKind", 3 )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  2 )

  ERR = MIOMD%SET( "step", ISTEP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  3 )

  ERR = MIO_HANDLE%FLUSH( MIOMD )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  4 )

  ERR = MIOMD%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  5 )


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create a new MultIO metadata object' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set \"flushKind\" in the MultIO metadata' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set \"step\" in the MultIO metadata' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to call the flush' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to delete MultIO metadata object' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_FLUSH_LAST_STEP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief send a flush to MultiIO using the provided handle.
!>
!> This function is used to send a flush message to the specified MultIO
!> handle. The flush contain also the information that this is the end
!> of the simulation (still not have a real utility, the idea is to use
!> this for deallocating all the data inside MultIO actions in order to
!> have more control over the destructor stage).
!>
!> @param [inout] mio_handle A MultIO handle
!>
!> @see MULTIO_FLUSH
!> @see MULTIO_FLUSH_AND_TRIGGER_RESTART
!> @see MULTIO_FLUSH_LAST_STEP
!> @see MULTIO_NOTIFY_STEP
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_FLUSH_END_OF_SIMULATION'
SUBROUTINE MULTIO_FLUSH_END_OF_SIMULATION(MIO_HANDLE)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: MULTIO_API,  ONLY: MULTIO_HANDLE
  USE :: MULTIO_API,  ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API,  ONLY: MULTIO_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE), INTENT(INOUT) :: MIO_HANDLE

  ! Local variables
  TYPE(MULTIO_METADATA) :: MIOMD
  INTEGER(KIND=JPIM_K)  :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()


  ERR = MIOMD%NEW(MIO_HANDLE)
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

  ERR = MIOMD%SET( "flushKind", 4 )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  2 )

  ERR = MIOMD%SET( "step", -1 )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  3 )

  ERR = MIO_HANDLE%FLUSH(MIOMD)
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  4 )

  ERR = MIOMD%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  5 )


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create a new MultIO metadata object' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set \"flushKind\" in the MultIO metadata' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set \"step\" in the MultIO metadata' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to call the flush' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to delete MultIO metadata object' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_FLUSH_END_OF_SIMULATION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief send a notify to MultiIO the current step.
!>
!> @param [in] mio_handle A MultIO handle
!> @param [in] step       The current step
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_NOTIFY_STEP'
SUBROUTINE MULTIO_NOTIFY_STEP(MIO_HANDLE, STEP)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: MULTIO_API,  ONLY: MULTIO_HANDLE
  USE :: MULTIO_API,  ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API,  ONLY: MULTIO_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE),  INTENT(INOUT) :: MIO_HANDLE
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: STEP

  ! Local variables
  TYPE(MULTIO_METADATA) :: MIOMD
  INTEGER(KIND=JPIM_K)  :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ERR = MIOMD%NEW(MIO_HANDLE)
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

  ERR = MIOMD%SET( "trigger", "step" )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  2 )

  ERR = MIOMD%SET( "step", STEP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  3 )

  ERR = MIO_HANDLE%NOTIFY(MIOMD)
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  4 )

  ERR = MIOMD%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  5 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create a new MultIO metadata object' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set \"flushKind\" in the MultIO metadata' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set \"step\" in the MultIO metadata' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to call the flush' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to delete MultIO metadata object' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_NOTIFY_STEP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_ERROR_HANDLER'
SUBROUTINE MULTIO_ERROR_HANDLER( CONTEXT, ERR, INFO )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_FAILURE_INFO
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=INT64),       INTENT(INOUT) :: CONTEXT
  INTEGER,                   INTENT(IN)    :: ERR
  TYPE(MULTIO_FAILURE_INFO), INTENT(IN)    :: INFO

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check error
  PP_DEBUG_CRITICAL_COND_THROW( ERR .NE. MULTIO_SUCCESS, 1 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      MIO_ERR_STR = MULTIO_ERROR_STRING( ERR, INFO )
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'MULTIO_UTILS_MOD:MULTIO_ERROR_HANDLER - Error: '//TRIM(MIO_ERR_STR) )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_ERROR_HANDLER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE OM_MULTIO_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
