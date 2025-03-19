!>
!> @file grib2_significanceOfReferenceTimeMap_mod.F90
!>
!> @brief Module for mapping GRIB2 Section 1 significanceOfReferenceTime.
!>
!>
!> @author Philipp Geier
!> @date   February, 2025
!>

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'grib2_significanceOfReferenceTimeMap_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB2_SIGNIFICANCEOFREFERENCETIME'
MODULE GRIB2_SIGNIFICANCEOFREFERENCETIME_MOD

  !> Symbols imported from other modules within the project.
  USE :: GRIB_SECTION_BASE_MOD, ONLY: GRIB_SECTION_BASE_A

IMPLICIT NONE

!>
!> Default symbols visibility
PRIVATE


!>
!> Public symbols (dataTypes)
PUBLIC :: SIGNIFICANCEOFREFERENCETIME_MAP

CONTAINS

!>
!> @brief Map MSG + PAR to significanceOfReferenceTime
!>
!>
!> @section interface
!>   @param [in]    MSG   All the mars keywords needed to describe the field `FORTRAN_MESSAGE_T`.
!>   @param [in]    PAR   All information outside mars keywords needed to describe the field `PARAMETRIZATION_T`.
!>   @param [out]   significanceOfReferenceTime (SORT)
!>   @param [inout] HOOKS A structure of type `HOOKS_T` that contains hooks for initialization.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>
!> @susection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'SIGNIFICANCEOFREFERENCETIME_MAP'
PP_THREAD_SAFE FUNCTION SIGNIFICANCEOFREFERENCETIME_MAP( &
&               MSG, PAR, SORT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  USE :: ENUMERATORS_MOD,          ONLY: TYPE_FG_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_AN_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_IA_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_OI_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_3V_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_4V_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_3G_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_4G_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_FC_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_CF_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_PF_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_EF_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_EA_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_CM_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_CS_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_FP_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_EM_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_ES_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_FA_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_CL_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_SI_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_S3_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_ED_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_TU_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_FF_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_OF_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_EFI_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_EFIC_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_PB_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_EP_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_BF_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_CD_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_4I_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_GO_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_ME_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_PD_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_CI_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_SOT_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_EME_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_IM_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_SIM_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_WEM_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_WES_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_CR_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_SES_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_TAEM_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_TAES_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_SG_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_SF_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_PA_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_ICP_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_SV_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_AS_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_SVAR_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_CV_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_OR_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_FX_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_FU_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_SFO_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_TPA_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_IF_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_FCMEAN_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_FCMAX_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_FCMIN_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_FCSTDEV_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_HCMEAN_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_SSD_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_GSD_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_GA_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_GAI_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_GBF_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_PFC_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_PPM_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_GWT_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_EST_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_MPP_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_OB_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_FB_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_AI_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_AF_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_AB_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_TF_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_MFB_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_OFB_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_OAI_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_SFB_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_FSOIFB_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_FCDFB_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),      INTENT(IN)    :: PAR
  INTEGER(KIND=JPIB_K),         INTENT(OUT)   :: SORT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNHANDLED_CASE=1_JPIB_K

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

  SELECT CASE ( MSG%TYPE )

  ! CASE( TYPE_FG_E )
  !   ! First guess
  !   SORT = 1_JPIB_K
  CASE( TYPE_AN_E )
    ! Analysis
    SORT = 0_JPIB_K
  CASE( TYPE_IA_E )
    ! Initialised analysis
    SORT = 0_JPIB_K
  CASE( TYPE_OI_E )
    ! Oi analysis
    SORT = 0_JPIB_K
  CASE( TYPE_3V_E )
    ! 3d variational analysis
    SORT = 0_JPIB_K
  CASE( TYPE_4V_E )
    ! 4d variational analysis
    SORT = 0_JPIB_K
  CASE( TYPE_3G_E )
    ! 3d variational gradients
    SORT = 0_JPIB_K
  CASE( TYPE_4G_E )
    ! 4d variational gradients
    SORT = 0_JPIB_K
  CASE( TYPE_FC_E )
    ! Forecast
    SORT = 1_JPIB_K
  CASE( TYPE_CF_E )
    ! Control forecast
    SORT = 1_JPIB_K
  CASE( TYPE_PF_E )
    ! Perturbed forecast
    SORT = 1_JPIB_K
  ! CASE( TYPE_EF_E )
  !   ! Errors in first guess
  !   SORT = 1_JPIB_K
  CASE( TYPE_EA_E )
    ! Errors in analysis
    SORT = 0_JPIB_K
  ! CASE( TYPE_CM_E )
  !   ! Cluster means
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_CS_E )
  !   ! Cluster std deviations
  !   SORT = 1_JPIB_K
  CASE( TYPE_FP_E )
    ! Forecast probability
    SORT = 1_JPIB_K
  CASE( TYPE_EM_E )
    ! Ensemble mean
    SORT = 1_JPIB_K
  CASE( TYPE_ES_E )
    ! Ensemble standard deviation
    SORT = 1_JPIB_K
  CASE( TYPE_FA_E )
    ! Forecast accumulation
    SORT = 1_JPIB_K
  ! CASE( TYPE_CL_E )
  !   ! Climatology
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_SI_E )
  !   ! Climate simulation
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_S3_E )
  !   ! Climate 30 days simulation
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_ED_E )
  !   ! Empirical distribution
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_TU_E )
  !   ! Tubes
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_FF_E )
  !   ! Flux forcing realtime
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_OF_E )
  !   ! Ocean forward
  !   SORT = 1_JPIB_K
  CASE( TYPE_EFI_E )
    ! Extreme forecast index
    SORT = 1_JPIB_K
  CASE( TYPE_EFIC_E )
    ! Extreme forecast index control
    SORT = 1_JPIB_K
  ! CASE( TYPE_PB_E )
  !   ! Probability boundaries
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_EP_E )
  !   ! Event probability
  !   SORT = 1_JPIB_K
  CASE( TYPE_BF_E )
    ! Bias-corrected forecast
    SORT = 1_JPIB_K
  ! CASE( TYPE_CD_E )
  !   ! Climate distribution
  !   SORT = 1_JPIB_K
  CASE( TYPE_4I_E )
    ! 4D analysis increments
    SORT = 0_JPIB_K
  ! CASE( TYPE_GO_E )
  !   ! Gridded observations
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_ME_E )
  !   ! Model errors
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_PD_E )
  !   ! Probability distribution
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_CI_E )
  !   ! Cluster information
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_SOT_E )
  !   ! Shift of Tail
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_EME_E )
  !   ! Ensemble data assimilation model errors
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_IM_E )
  !   ! Images
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_SIM_E )
  !   ! Simulated images
  !   SORT = 1_JPIB_K
  CASE( TYPE_WEM_E )
    ! Weighted ensemble mean
    SORT = 1_JPIB_K
  CASE( TYPE_WES_E )
    ! Weighted ensemble standard deviation
    SORT = 1_JPIB_K
  ! CASE( TYPE_CR_E )
  !   ! Cluster representative
  !   SORT = 1_JPIB_K
  CASE( TYPE_SES_E )
    ! Scaled ensemble standard deviation
    SORT = 1_JPIB_K
  CASE( TYPE_TAEM_E )
    ! Time average ensemble mean
    SORT = 1_JPIB_K
  CASE( TYPE_TAES_E )
    ! Time average ensemble standard deviation
    SORT = 1_JPIB_K
  CASE( TYPE_SG_E )
    ! Sensitivity gradient
    SORT = 1_JPIB_K
  CASE( TYPE_SF_E )
    ! Sensitivity forecast
    SORT = 1_JPIB_K
  CASE( TYPE_PA_E )
    ! Perturbed analysis
    SORT = 0_JPIB_K
  ! CASE( TYPE_ICP_E )
  !   ! Initial condition perturbation
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_SV_E )
  !   ! Singular vector
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_AS_E )
  !   ! Adjoint singular vector
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_SVAR_E )
  !   ! Signal variance
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_CV_E )
  !   ! Calibration/Validation forecast
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_OR_E )
  !   ! Ocean reanalysis
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_FX_E )
  !   ! Flux forcing
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_FU_E )
  !   ! Fill-up
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_SFO_E )
  !   ! Simulations with forcing
  !   SORT = 1_JPIB_K
  CASE( TYPE_TPA_E )
    ! Time processed analysis
    SORT = 0_JPIB_K
  CASE( TYPE_IF_E )
    ! Interim forecast
    SORT = 1_JPIB_K
  CASE( TYPE_FCMEAN_E )
    ! Forecast mean
    SORT = 1_JPIB_K
  CASE( TYPE_FCMAX_E )
    ! Forecast maximum
    SORT = 1_JPIB_K
  CASE( TYPE_FCMIN_E )
    ! Forecast minimum
    SORT = 1_JPIB_K
  CASE( TYPE_FCSTDEV_E )
    ! Forecast standard deviation
    SORT = 1_JPIB_K
  ! CASE( TYPE_HCMEAN_E )
  !   ! Hindcast climate mean
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_SSD_E )
  !   ! Simulated satellite data
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_GSD_E )
  !   ! Gridded satellite data
  !   SORT = 1_JPIB_K
  CASE( TYPE_GA_E )
    ! GFAS analysis
    SORT = 0_JPIB_K
  CASE( TYPE_GAI_E )
    ! Gridded analysis input
    SORT = 0_JPIB_K
  ! CASE( TYPE_GBF_E )
  !   ! Bias-corrected gridbox
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_PFC_E )
  !   ! Point values
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_PPM_E )
  !   ! Point value metrics
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_GWT_E )
  !   ! Weather types
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_EST_E )
  !   ! Ensemble statistics
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_MPP_E )
  !   ! Model physics perturbations
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_OB_E )
  !   ! Observations
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_FB_E )
  !   ! Feedback
  !   SORT = 1_JPIB_K
  CASE( TYPE_AI_E )
    ! Analysis input
    SORT = 0_JPIB_K
  CASE( TYPE_AF_E )
    ! Analysis feedback
    SORT = 0_JPIB_K
  CASE( TYPE_AB_E )
    ! Analysis bias
    SORT = 0_JPIB_K
  CASE( TYPE_TF_E )
    ! Trajectory forecast
    SORT = 1_JPIB_K
  ! CASE( TYPE_MFB_E )
  !   ! MonDB feedback
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_OFB_E )
  !   ! ODB feedback
  !   SORT = 1_JPIB_K
  CASE( TYPE_OAI_E )
    ! ODB analysis input
    SORT = 0_JPIB_K
  ! CASE( TYPE_SFB_E )
  !   ! Summary feedback
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_FSOIFB_E )
  !   ! Forecast sensitivity to observations impact feedback
  !   SORT = 1_JPIB_K
  ! CASE( TYPE_FCDFB_E )
  !   ! Forecast departures feedback
  !   SORT = 1_JPIB_K


  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNHANDLED_CASE )

  END SELECT


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
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
    CASE ( ERRFLAG_UNHANDLED_CASE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled MARS type' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
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

END FUNCTION SIGNIFICANCEOFREFERENCETIME_MAP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE GRIB2_SIGNIFICANCEOFREFERENCETIME_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
