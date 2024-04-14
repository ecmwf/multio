!> @file grib_encoder_manager_mod.F90
!>
!> @brief Organize encoders in a hash table
!>
!> Encoders don't have factories, all the available encoders are
!> organised in a hash table. All of them are constructed at the
!> startup of the output manager and destroied at the end.
!>
!> Moreover, in this module, it is defined a container for managing instances
!> of GRIB encoders. It encapsulates procedures and data related to the creation,
!> encoding, and deallocation of GRIB encoders. The class includes private pointers
!> for functions to make and destroy encoders, as well as a pointer to the actual
!> GRIB encoder instance.
!>
!> @author Mirco Valentini
!> @date January 18, 2024

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'grib_encoder_manager_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB_ENCODER_MANAGER_MOD'
MODULE GRIB_ENCODER_MANAGER_MOD

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,           ONLY: JPIB_K
  USE :: GRIB_ENCODER_BASE_MOD, ONLY: GRIB_ENCODER_A

IMPLICIT NONE

! Default visibility
PRIVATE


INTERFACE

!>
!> @brief interface used for the construction of a generic encoders
!>
!> @param [in] MODEL_PARAMS   Model parameters that are frozen during the simulation
!> @param [in] ENCODER        Encoder to be allocated
!>
!> @result The newly contructed encoder
!>
SUBROUTINE MAKE_FCN_IF( CFG, MODEL_PARAMS, METADATA_KIND, ENCODER, MIOH )
  USE :: OM_CORE_MOD,           ONLY: MODEL_PAR_T
  USE :: GRIB_ENCODER_BASE_MOD, ONLY: GRIB_ENCODER_A
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION
  USE :: MULTIO_API,                 ONLY: MULTIO_HANDLE
IMPLICIT NONE
  TYPE(FCKIT_CONFIGURATION),      INTENT(IN)    :: CFG
  TYPE(MODEL_PAR_T),              INTENT(IN)    :: MODEL_PARAMS
  CHARACTER(LEN=*),               INTENT(IN)    :: METADATA_KIND
  CLASS(GRIB_ENCODER_A), POINTER, INTENT(INOUT) :: ENCODER
  TYPE(MULTIO_HANDLE), OPTIONAL,  INTENT(IN)    :: MIOH
END SUBROUTINE MAKE_FCN_IF


!>
!> @brief interface used for the destruction of an encoder
!>
!> @param [ioout] the encoder to be destructed
!>
SUBROUTINE DESTROY_FCN_IF( ENCODER )
  USE :: OM_CORE_MOD,           ONLY: JPIB_K
  USE :: GRIB_ENCODER_BASE_MOD, ONLY: GRIB_ENCODER_A
IMPLICIT NONE
  CLASS(GRIB_ENCODER_A), POINTER, INTENT(INOUT) :: ENCODER
END SUBROUTINE DESTROY_FCN_IF
END INTERFACE


!> @brief Container for the Encoders
!>
!> This type, `GRIB_ENCODER_CONTAINER_T`, serves as a container for managing instances
!> of GRIB encoders. It encapsulates procedures and data related to the creation,
!> encoding, and deallocation of GRIB encoders.
!>
TYPE :: GRIB_ENCODER_CONTAINER_T

  ! Default visibility of the members of the class
  PRIVATE

  !> @brief Private pointer to the make function interface for creating encoders.
  PROCEDURE(MAKE_FCN_IF), POINTER, NOPASS :: MAKE_FCN_ => NULL()

  !> @brief Private pointer to the destroy function interface for deallocating encoders.
  PROCEDURE(DESTROY_FCN_IF), POINTER, NOPASS :: DESTROY_FCN_ => NULL()

  !> @brief Pointer to the instance of the GRIB encoder.
  CLASS(GRIB_ENCODER_A), POINTER :: ENCODER_ => NULL()
CONTAINS

  !> @brief Public method used to create an instance of the encoder container.
  PROCEDURE, PASS, NON_OVERRIDABLE, PUBLIC :: CREATE => CONTAINER_CREATE

  !> @brief Public method for encoding atmpshere data using the encapsulated GRIB encoder.
  PROCEDURE, PASS, NON_OVERRIDABLE, PUBLIC :: ENCODE_ATM => CONTAINER_ENCODE_ATM

  !> @brief Public method for encoding wave data using the encapsulated GRIB encoder.
  PROCEDURE, PASS, NON_OVERRIDABLE, PUBLIC :: ENCODE_WAM => CONTAINER_ENCODE_WAM

  !> @brief Public method to deallocate resources associated with the encoder container.
  PROCEDURE, PASS, NON_OVERRIDABLE, PUBLIC :: DEALLOCATE => CONTAINER_DEALLOCATE
END TYPE


!> @brief Global hash table
TYPE(GRIB_ENCODER_CONTAINER_T), DIMENSION(:), ALLOCATABLE :: ENCODERS

!> @brief Configurations that comes from the yaml configuration file
LOGICAL :: LGENABLE_GRIB1_GG_M  = .TRUE.
LOGICAL :: LGENABLE_GRIB1_GG_P  = .TRUE.
LOGICAL :: LGENABLE_GRIB1_GG_T  = .TRUE.
LOGICAL :: LGENABLE_GRIB1_GG_V  = .TRUE.
LOGICAL :: LGENABLE_GRIB1_GG_S  = .TRUE.
LOGICAL :: LGENABLE_GRIB1_GG_WI = .TRUE.
LOGICAL :: LGENABLE_GRIB1_GG_WS = .TRUE.
LOGICAL :: LGENABLE_GRIB1_SH_M  = .TRUE.
LOGICAL :: LGENABLE_GRIB1_SH_P  = .TRUE.
LOGICAL :: LGENABLE_GRIB1_SH_T  = .TRUE.
LOGICAL :: LGENABLE_GRIB1_SH_V  = .TRUE.
LOGICAL :: LGENABLE_GRIB1_SH_S  = .TRUE.
LOGICAL :: LGENABLE_GRIB2_GG_M  = .TRUE.
LOGICAL :: LGENABLE_GRIB2_GG_P  = .TRUE.
LOGICAL :: LGENABLE_GRIB2_GG_T  = .TRUE.
LOGICAL :: LGENABLE_GRIB2_GG_V  = .TRUE.
LOGICAL :: LGENABLE_GRIB2_GG_S  = .TRUE.
LOGICAL :: LGENABLE_GRIB2_GG_WI = .TRUE.
LOGICAL :: LGENABLE_GRIB2_GG_WS = .TRUE.
LOGICAL :: LGENABLE_GRIB2_SH_M  = .TRUE.
LOGICAL :: LGENABLE_GRIB2_SH_P  = .TRUE.
LOGICAL :: LGENABLE_GRIB2_SH_T  = .TRUE.
LOGICAL :: LGENABLE_GRIB2_SH_V  = .TRUE.
LOGICAL :: LGENABLE_GRIB2_SH_S  = .TRUE.



!> @breief Dimensions used to allocate the encoders
INTEGER(KIND=JPIB_K), PARAMETER :: NREPRES=2
INTEGER(KIND=JPIB_K), PARAMETER :: NEDITION=2
INTEGER(KIND=JPIB_K), PARAMETER :: NPREFIX=8


! Whitelist of public symbols
PUBLIC :: MAKE_ENCODERS
PUBLIC :: DESTROY_ENCODERS
PUBLIC :: GRIB_ENCODER_CONTAINER_T
PUBLIC :: ENCODE_WAM
PUBLIC :: ENCODE_ATM


CONTAINS


!>
!> @brief Destroy the encoder's table
!>
!> This subroutine is responsible for finlising and destroyng the encoder's table.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SUENCODER_MANAGER'
SUBROUTINE SUENCODER_MANAGER( CFG, MODEL_PARAMS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Initialisation of utils
  USE :: ENCODERS_UTILS_MOD, ONLY: UTILS_FREE

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FCKIT_CONFIGURATION), INTENT(IN) :: CFG
  TYPE(MODEL_PAR_T),         INTENT(IN) :: MODEL_PARAMS

  ! Local variables
  TYPE(FCKIT_CONFIGURATION) :: EMCFG
  LOGICAL :: LTMP
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( CFG%GET( 'encoder-manager', EMCFG ) ) THEN

    ! Read enable-grib1-gg-m
    IF ( EMCFG%GET( 'enable-grib1-gg-m', LTMP ) ) THEN
      LGENABLE_GRIB1_GG_M = LTMP
    ELSE
      LGENABLE_GRIB1_GG_M = .TRUE.
    ENDIF

    ! Read enable-grib1-gg-p
    IF ( EMCFG%GET( 'enable-grib1-gg-p', LTMP ) ) THEN
      LGENABLE_GRIB1_GG_P = LTMP
    ELSE
      LGENABLE_GRIB1_GG_P = .TRUE.
    ENDIF

    ! Read enable-grib1-gg-t
    IF ( EMCFG%GET( 'enable-grib1-gg-t', LTMP ) ) THEN
      LGENABLE_GRIB1_GG_T = LTMP
    ELSE
      LGENABLE_GRIB1_GG_T = .TRUE.
    ENDIF

    ! Read enable-grib1-gg-v
    IF ( EMCFG%GET( 'enable-grib1-gg-v', LTMP ) ) THEN
      LGENABLE_GRIB1_GG_V = LTMP
    ELSE
      LGENABLE_GRIB1_GG_V = .TRUE.
    ENDIF

    ! Read enable-grib1-gg-s
    IF ( EMCFG%GET( 'enable-grib1-gg-s', LTMP ) ) THEN
      LGENABLE_GRIB1_GG_S = LTMP
    ELSE
      LGENABLE_GRIB1_GG_S = .TRUE.
    ENDIF

    ! Read enable-grib1-gg-wi
    IF ( EMCFG%GET( 'enable-grib1-gg-wi', LTMP ) ) THEN
      LGENABLE_GRIB1_GG_WI = LTMP
    ELSE
      LGENABLE_GRIB1_GG_WI = .TRUE.
    ENDIF

    ! Read enable-grib1-gg-ws
    IF ( EMCFG%GET( 'enable-grib1-gg-ws', LTMP ) ) THEN
      LGENABLE_GRIB1_GG_WS = LTMP
    ELSE
      LGENABLE_GRIB1_GG_WS = .TRUE.
    ENDIF

    ! Read enable-grib1-sh-m
    IF ( EMCFG%GET( 'enable-grib1-sh-m', LTMP ) ) THEN
      LGENABLE_GRIB1_SH_M = LTMP
    ELSE
      LGENABLE_GRIB1_SH_M = .TRUE.
    ENDIF

    ! Read enable-grib1-sh-p
    IF ( EMCFG%GET( 'enable-grib1-sh-p', LTMP ) ) THEN
      LGENABLE_GRIB1_SH_P = LTMP
    ELSE
      LGENABLE_GRIB1_SH_P = .TRUE.
    ENDIF

    ! Read enable-grib1-sh-t
    IF ( EMCFG%GET( 'enable-grib1-sh-t', LTMP ) ) THEN
      LGENABLE_GRIB1_SH_T = LTMP
    ELSE
      LGENABLE_GRIB1_SH_T = .TRUE.
    ENDIF

    ! Read enable-grib1-sh-v
    IF ( EMCFG%GET( 'enable-grib1-sh-v', LTMP ) ) THEN
      LGENABLE_GRIB1_SH_V = LTMP
    ELSE
      LGENABLE_GRIB1_SH_V = .TRUE.
    ENDIF

    ! Read enable-grib1-sh-s
    IF ( EMCFG%GET( 'enable-grib1-sh-s', LTMP ) ) THEN
      LGENABLE_GRIB1_SH_S = LTMP
    ELSE
      LGENABLE_GRIB1_SH_S = .TRUE.
    ENDIF


    ! Read enable-grib2-gg-m
    IF ( EMCFG%GET( 'enable-grib2-gg-m', LTMP ) ) THEN
      LGENABLE_GRIB2_GG_M = LTMP
    ELSE
      LGENABLE_GRIB2_GG_M = .TRUE.
    ENDIF

    ! Read enable-grib2-gg-p
    IF ( EMCFG%GET( 'enable-grib2-gg-p', LTMP ) ) THEN
      LGENABLE_GRIB2_GG_P = LTMP
    ELSE
      LGENABLE_GRIB2_GG_P = .TRUE.
    ENDIF

    ! Read enable-grib2-gg-t
    IF ( EMCFG%GET( 'enable-grib2-gg-t', LTMP ) ) THEN
      LGENABLE_GRIB2_GG_T = LTMP
    ELSE
      LGENABLE_GRIB2_GG_T = .TRUE.
    ENDIF

    ! Read enable-grib2-gg-v
    IF ( EMCFG%GET( 'enable-grib2-gg-v', LTMP ) ) THEN
      LGENABLE_GRIB2_GG_V = LTMP
    ELSE
      LGENABLE_GRIB2_GG_V = .TRUE.
    ENDIF

    ! Read enable-grib2-gg-s
    IF ( EMCFG%GET( 'enable-grib2-gg-s', LTMP ) ) THEN
      LGENABLE_GRIB2_GG_S = LTMP
    ELSE
      LGENABLE_GRIB2_GG_S = .TRUE.
    ENDIF

    ! Read enable-grib2-gg-wi
    IF ( EMCFG%GET( 'enable-grib2-gg-wi', LTMP ) ) THEN
      LGENABLE_GRIB2_GG_WI = LTMP
    ELSE
      LGENABLE_GRIB2_GG_WI = .TRUE.
    ENDIF

    ! Read enable-grib2-gg-ws
    IF ( EMCFG%GET( 'enable-grib2-gg-ws', LTMP ) ) THEN
      LGENABLE_GRIB2_GG_WS = LTMP
    ELSE
      LGENABLE_GRIB2_GG_WS = .TRUE.
    ENDIF

    ! Read enable-grib2-sh-m
    IF ( EMCFG%GET( 'enable-grib2-sh-m', LTMP ) ) THEN
      LGENABLE_GRIB2_SH_M = LTMP
    ELSE
      LGENABLE_GRIB2_SH_M = .TRUE.
    ENDIF

    ! Read enable-grib2-sh-p
    IF ( EMCFG%GET( 'enable-grib2-sh-p', LTMP ) ) THEN
      LGENABLE_GRIB2_SH_P = LTMP
    ELSE
      LGENABLE_GRIB2_SH_P = .TRUE.
    ENDIF

    ! Read enable-grib2-sh-t
    IF ( EMCFG%GET( 'enable-grib2-sh-t', LTMP ) ) THEN
      LGENABLE_GRIB2_SH_T = LTMP
    ELSE
      LGENABLE_GRIB2_SH_T = .TRUE.
    ENDIF

    ! Read enable-grib2-sh-v
    IF ( EMCFG%GET( 'enable-grib2-sh-v', LTMP ) ) THEN
      LGENABLE_GRIB2_SH_V = LTMP
    ELSE
      LGENABLE_GRIB2_SH_V = .TRUE.
    ENDIF

    ! Read enable-grib2-sh-s
    IF ( EMCFG%GET( 'enable-grib2-sh-s', LTMP ) ) THEN
      LGENABLE_GRIB2_SH_S = LTMP
    ELSE
      LGENABLE_GRIB2_SH_S = .TRUE.
    ENDIF

  ENDIF

  CALL EMCFG%FINAL()


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SUENCODER_MANAGER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Create the encoder's table
!>
!> This function is responsible for creating and initializing the encoder's table.
!>
!> @param [in] PROCESSOR_TOPO Processor topology to be used in a multiprocessor run
!> @param [in] MODEL_PARAMS   Model parameters that are frozen during the simulation
!> @param [in] ctype          Type of the metadata to be used internally [grib|multio]
!> @param [in] optionalData   Optional data used as additional argument to initialize metadata
!>
!> @result An array of encoders, allocated and initialized.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAKE_ENCODERS'
SUBROUTINE MAKE_ENCODERS( CFG, MODEL_PARAMS, METADATA_KIND, MIOH )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,              ONLY: MODEL_PAR_T
  USE :: GRIB_ENCODER_INDEXER_MOD, ONLY: ENCODERS_HASH_SIZE
  USE :: GRIB_ENCODER_INDEXER_MOD, ONLY: ENCODER_IDX

  USE :: OM_CORE_MOD, ONLY: MODEL_LEVEL_E
  USE :: OM_CORE_MOD, ONLY: PRESSURE_LEVEL_E
  USE :: OM_CORE_MOD, ONLY: VORTICITY_LEVEL_E
  USE :: OM_CORE_MOD, ONLY: THETA_LEVEL_E
  USE :: OM_CORE_MOD, ONLY: SURFACE_E
  USE :: OM_CORE_MOD, ONLY: WAVE_INT_E
  USE :: OM_CORE_MOD, ONLY: WAVE_SPEC_E
  USE :: OM_CORE_MOD, ONLY: REPRES_GRIDDED_E
  USE :: OM_CORE_MOD, ONLY: REPRES_SPECTRAL_E
  USE :: OM_CORE_MOD, ONLY: GRIB1_E
  USE :: OM_CORE_MOD, ONLY: GRIB2_E

  ! Initialisation of utils
  USE :: ENCODERS_UTILS_MOD, ONLY: UTILS_INIT

  ! Encoders constructors and destructors
  USE :: GRIB_ENCODER_BASE_MOD, ONLY: GRIB_ENCODER_A

  ! Grib1 gridded encoders
  USE :: GRIB_ENCODER_GRIB1_GG_P_MOD,  ONLY: MAKE_GRIB1_GG_P_ENCODER
  USE :: GRIB_ENCODER_GRIB1_GG_P_MOD,  ONLY: DESTROY_GRIB1_GG_P_ENCODER
  USE :: GRIB_ENCODER_GRIB1_GG_T_MOD,  ONLY: MAKE_GRIB1_GG_T_ENCODER
  USE :: GRIB_ENCODER_GRIB1_GG_T_MOD,  ONLY: DESTROY_GRIB1_GG_T_ENCODER
  USE :: GRIB_ENCODER_GRIB1_GG_V_MOD,  ONLY: MAKE_GRIB1_GG_V_ENCODER
  USE :: GRIB_ENCODER_GRIB1_GG_V_MOD,  ONLY: DESTROY_GRIB1_GG_V_ENCODER
  USE :: GRIB_ENCODER_GRIB1_GG_S_MOD,  ONLY: MAKE_GRIB1_GG_S_ENCODER
  USE :: GRIB_ENCODER_GRIB1_GG_S_MOD,  ONLY: DESTROY_GRIB1_GG_S_ENCODER
  USE :: GRIB_ENCODER_GRIB1_GG_M_MOD,  ONLY: MAKE_GRIB1_GG_M_ENCODER
  USE :: GRIB_ENCODER_GRIB1_GG_M_MOD,  ONLY: DESTROY_GRIB1_GG_M_ENCODER
  USE :: GRIB_ENCODER_GRIB1_GG_WI_MOD, ONLY: MAKE_GRIB1_GG_WI_ENCODER
  USE :: GRIB_ENCODER_GRIB1_GG_WI_MOD, ONLY: DESTROY_GRIB1_GG_WI_ENCODER
  USE :: GRIB_ENCODER_GRIB1_GG_WS_MOD, ONLY: MAKE_GRIB1_GG_WS_ENCODER
  USE :: GRIB_ENCODER_GRIB1_GG_WS_MOD, ONLY: DESTROY_GRIB1_GG_WS_ENCODER

  ! Grib1 shperical harmonics encoders
  USE :: GRIB_ENCODER_GRIB1_SH_P_MOD,  ONLY: MAKE_GRIB1_SH_P_ENCODER
  USE :: GRIB_ENCODER_GRIB1_SH_P_MOD,  ONLY: DESTROY_GRIB1_SH_P_ENCODER
  USE :: GRIB_ENCODER_GRIB1_SH_T_MOD,  ONLY: MAKE_GRIB1_SH_T_ENCODER
  USE :: GRIB_ENCODER_GRIB1_SH_T_MOD,  ONLY: DESTROY_GRIB1_SH_T_ENCODER
  USE :: GRIB_ENCODER_GRIB1_SH_V_MOD,  ONLY: MAKE_GRIB1_SH_V_ENCODER
  USE :: GRIB_ENCODER_GRIB1_SH_V_MOD,  ONLY: DESTROY_GRIB1_SH_V_ENCODER
  USE :: GRIB_ENCODER_GRIB1_SH_S_MOD,  ONLY: MAKE_GRIB1_SH_S_ENCODER
  USE :: GRIB_ENCODER_GRIB1_SH_S_MOD,  ONLY: DESTROY_GRIB1_SH_S_ENCODER
  USE :: GRIB_ENCODER_GRIB1_SH_M_MOD,  ONLY: MAKE_GRIB1_SH_M_ENCODER
  USE :: GRIB_ENCODER_GRIB1_SH_M_MOD,  ONLY: DESTROY_GRIB1_SH_M_ENCODER

  ! Grib2 gridded encoders
  USE :: GRIB_ENCODER_GRIB2_GG_P_MOD,  ONLY: MAKE_GRIB2_GG_P_ENCODER
  USE :: GRIB_ENCODER_GRIB2_GG_P_MOD,  ONLY: DESTROY_GRIB2_GG_P_ENCODER
  USE :: GRIB_ENCODER_GRIB2_GG_T_MOD,  ONLY: MAKE_GRIB2_GG_T_ENCODER
  USE :: GRIB_ENCODER_GRIB2_GG_T_MOD,  ONLY: DESTROY_GRIB2_GG_T_ENCODER
  USE :: GRIB_ENCODER_GRIB2_GG_V_MOD,  ONLY: MAKE_GRIB2_GG_V_ENCODER
  USE :: GRIB_ENCODER_GRIB2_GG_V_MOD,  ONLY: DESTROY_GRIB2_GG_V_ENCODER
  USE :: GRIB_ENCODER_GRIB2_GG_S_MOD,  ONLY: MAKE_GRIB2_GG_S_ENCODER
  USE :: GRIB_ENCODER_GRIB2_GG_S_MOD,  ONLY: DESTROY_GRIB2_GG_S_ENCODER
  USE :: GRIB_ENCODER_GRIB2_GG_M_MOD,  ONLY: MAKE_GRIB2_GG_M_ENCODER
  USE :: GRIB_ENCODER_GRIB2_GG_M_MOD,  ONLY: DESTROY_GRIB2_GG_M_ENCODER
  USE :: GRIB_ENCODER_GRIB2_GG_WI_MOD, ONLY: MAKE_GRIB2_GG_WI_ENCODER
  USE :: GRIB_ENCODER_GRIB2_GG_WI_MOD, ONLY: DESTROY_GRIB2_GG_WI_ENCODER
  USE :: GRIB_ENCODER_GRIB2_GG_WS_MOD, ONLY: MAKE_GRIB2_GG_WS_ENCODER
  USE :: GRIB_ENCODER_GRIB2_GG_WS_MOD, ONLY: DESTROY_GRIB2_GG_WS_ENCODER

  ! Grib2 shperical harmonics encoders
  USE :: GRIB_ENCODER_GRIB2_SH_P_MOD,  ONLY: MAKE_GRIB2_SH_P_ENCODER
  USE :: GRIB_ENCODER_GRIB2_SH_P_MOD,  ONLY: DESTROY_GRIB2_SH_P_ENCODER
  USE :: GRIB_ENCODER_GRIB2_SH_T_MOD,  ONLY: MAKE_GRIB2_SH_T_ENCODER
  USE :: GRIB_ENCODER_GRIB2_SH_T_MOD,  ONLY: DESTROY_GRIB2_SH_T_ENCODER
  USE :: GRIB_ENCODER_GRIB2_SH_V_MOD,  ONLY: MAKE_GRIB2_SH_V_ENCODER
  USE :: GRIB_ENCODER_GRIB2_SH_V_MOD,  ONLY: DESTROY_GRIB2_SH_V_ENCODER
  USE :: GRIB_ENCODER_GRIB2_SH_S_MOD,  ONLY: MAKE_GRIB2_SH_S_ENCODER
  USE :: GRIB_ENCODER_GRIB2_SH_S_MOD,  ONLY: DESTROY_GRIB2_SH_S_ENCODER
  USE :: GRIB_ENCODER_GRIB2_SH_M_MOD,  ONLY: MAKE_GRIB2_SH_M_ENCODER
  USE :: GRIB_ENCODER_GRIB2_SH_M_MOD,  ONLY: DESTROY_GRIB2_SH_M_ENCODER

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION
  USE :: MULTIO_API,                 ONLY: MULTIO_HANDLE

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FCKIT_CONFIGURATION),     INTENT(IN) :: CFG
  TYPE(MODEL_PAR_T),             INTENT(IN) :: MODEL_PARAMS
  CHARACTER(LEN=*),              INTENT(IN) :: METADATA_KIND
  TYPE(MULTIO_HANDLE), OPTIONAL, INTENT(IN) :: MIOH

  ! Local variables
  CLASS(GRIB_ENCODER_A), POINTER :: TMP
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  PROCEDURE(MAKE_FCN_IF),    POINTER :: MAKE_FCN_P
  PROCEDURE(DESTROY_FCN_IF), POINTER :: DESTROY_FCN_P

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization
  STAT = 0
  ERRIDX = 0
  ERRMSG = ''

  ! Configure the encoder manager
  CALL SUENCODER_MANAGER( CFG, MODEL_PARAMS )

  ! Crete the encoders
  IF ( .NOT.ALLOCATED(ENCODERS) ) THEN

    ! Initialise all the utils modules
    CALL UTILS_INIT( CFG, MODEL_PARAMS )

    ! Allocate encoders
    ALLOCATE( ENCODERS(ENCODERS_HASH_SIZE( )), STAT=STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 1 )

    ! Create the custom encoders

    ! Grib1 gridded encoders
    IF ( PRESENT(MIOH) ) THEN

      IF ( LGENABLE_GRIB1_GG_P ) THEN
        CALL ENCODERS( ENCODER_IDX( PRESSURE_LEVEL_E,  REPRES_GRIDDED_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_GG_P_ENCODER,  DESTROY_GRIB1_GG_P_ENCODER, MIOH=MIOH )
      ENDIF
      IF ( LGENABLE_GRIB1_GG_T ) THEN
        CALL ENCODERS( ENCODER_IDX( THETA_LEVEL_E,     REPRES_GRIDDED_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_GG_T_ENCODER,  DESTROY_GRIB1_GG_T_ENCODER, MIOH=MIOH )
      ENDIF
      IF ( LGENABLE_GRIB1_GG_V ) THEN
        CALL ENCODERS( ENCODER_IDX( VORTICITY_LEVEL_E, REPRES_GRIDDED_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_GG_V_ENCODER,  DESTROY_GRIB1_GG_V_ENCODER, MIOH=MIOH )
      ENDIF
      IF ( LGENABLE_GRIB1_GG_S ) THEN
        CALL ENCODERS( ENCODER_IDX( SURFACE_E,         REPRES_GRIDDED_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_GG_S_ENCODER,  DESTROY_GRIB1_GG_S_ENCODER, MIOH=MIOH )
      ENDIF
      IF ( LGENABLE_GRIB1_GG_M ) THEN
        CALL ENCODERS( ENCODER_IDX( MODEL_LEVEL_E,     REPRES_GRIDDED_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_GG_M_ENCODER,  DESTROY_GRIB1_GG_M_ENCODER, MIOH=MIOH )
      ENDIF
      IF ( LGENABLE_GRIB1_GG_WI ) THEN
        CALL ENCODERS( ENCODER_IDX( WAVE_INT_E,        REPRES_GRIDDED_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_GG_WI_ENCODER, DESTROY_GRIB1_GG_WI_ENCODER, MIOH=MIOH )
      ENDIF
      IF ( LGENABLE_GRIB1_GG_WS ) THEN
        CALL ENCODERS( ENCODER_IDX( WAVE_SPEC_E,       REPRES_GRIDDED_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_GG_WS_ENCODER, DESTROY_GRIB1_GG_WS_ENCODER, MIOH=MIOH )
      ENDIF

      ! Grib1 spherical harmonics encoders
      IF ( LGENABLE_GRIB1_SH_P ) THEN
        CALL ENCODERS( ENCODER_IDX( PRESSURE_LEVEL_E,  REPRES_SPECTRAL_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_SH_P_ENCODER,  DESTROY_GRIB1_SH_P_ENCODER, MIOH=MIOH )
      ENDIF
      IF ( LGENABLE_GRIB1_SH_T ) THEN
        CALL ENCODERS( ENCODER_IDX( THETA_LEVEL_E,     REPRES_SPECTRAL_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_SH_T_ENCODER,  DESTROY_GRIB1_SH_T_ENCODER, MIOH=MIOH )
      ENDIF
      IF ( LGENABLE_GRIB1_SH_V ) THEN
        CALL ENCODERS( ENCODER_IDX( VORTICITY_LEVEL_E, REPRES_SPECTRAL_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_SH_V_ENCODER,  DESTROY_GRIB1_SH_V_ENCODER, MIOH=MIOH )
      ENDIF
      IF ( LGENABLE_GRIB1_SH_S ) THEN
        CALL ENCODERS( ENCODER_IDX( SURFACE_E,         REPRES_SPECTRAL_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_SH_S_ENCODER,  DESTROY_GRIB1_SH_S_ENCODER, MIOH=MIOH )
      ENDIF
      IF ( LGENABLE_GRIB1_SH_M ) THEN
        CALL ENCODERS( ENCODER_IDX( MODEL_LEVEL_E,     REPRES_SPECTRAL_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_SH_M_ENCODER,  DESTROY_GRIB1_SH_M_ENCODER, MIOH=MIOH )
      ENDIF

      ! Grib2 gridded encoders
      IF ( LGENABLE_GRIB2_GG_P ) THEN
        CALL ENCODERS( ENCODER_IDX( PRESSURE_LEVEL_E,  REPRES_GRIDDED_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_GG_P_ENCODER,  DESTROY_GRIB2_GG_P_ENCODER, MIOH=MIOH )
      ENDIF
      IF ( LGENABLE_GRIB2_GG_T ) THEN
        CALL ENCODERS( ENCODER_IDX( THETA_LEVEL_E,     REPRES_GRIDDED_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_GG_T_ENCODER,  DESTROY_GRIB2_GG_T_ENCODER, MIOH=MIOH )
      ENDIF
      IF ( LGENABLE_GRIB2_GG_V ) THEN
        CALL ENCODERS( ENCODER_IDX( VORTICITY_LEVEL_E, REPRES_GRIDDED_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_GG_V_ENCODER,  DESTROY_GRIB2_GG_V_ENCODER, MIOH=MIOH )
      ENDIF
      IF ( LGENABLE_GRIB2_GG_S ) THEN
        CALL ENCODERS( ENCODER_IDX( SURFACE_E,         REPRES_GRIDDED_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_GG_S_ENCODER,  DESTROY_GRIB2_GG_S_ENCODER, MIOH=MIOH )
      ENDIF
      IF ( LGENABLE_GRIB2_GG_M ) THEN
        CALL ENCODERS( ENCODER_IDX( MODEL_LEVEL_E,     REPRES_GRIDDED_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_GG_M_ENCODER,  DESTROY_GRIB2_GG_M_ENCODER, MIOH=MIOH )
      ENDIF
      IF ( LGENABLE_GRIB2_GG_WI ) THEN
        CALL ENCODERS( ENCODER_IDX( WAVE_INT_E,        REPRES_GRIDDED_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_GG_WI_ENCODER, DESTROY_GRIB2_GG_WI_ENCODER, MIOH=MIOH )
      ENDIF
      IF ( LGENABLE_GRIB2_GG_WS ) THEN
        CALL ENCODERS( ENCODER_IDX( WAVE_SPEC_E,       REPRES_GRIDDED_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_GG_WS_ENCODER, DESTROY_GRIB2_GG_WS_ENCODER, MIOH=MIOH )
      ENDIF

      ! Grib2 gridded encoders
      IF ( LGENABLE_GRIB2_SH_P ) THEN
        CALL ENCODERS( ENCODER_IDX( PRESSURE_LEVEL_E,  REPRES_SPECTRAL_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_SH_P_ENCODER,  DESTROY_GRIB2_SH_P_ENCODER, MIOH=MIOH )
      ENDIF
      IF ( LGENABLE_GRIB2_SH_T ) THEN
        CALL ENCODERS( ENCODER_IDX( THETA_LEVEL_E,     REPRES_SPECTRAL_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_SH_T_ENCODER,  DESTROY_GRIB2_SH_T_ENCODER, MIOH=MIOH )
      ENDIF
      IF ( LGENABLE_GRIB2_SH_V ) THEN
        CALL ENCODERS( ENCODER_IDX( VORTICITY_LEVEL_E, REPRES_SPECTRAL_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_SH_V_ENCODER,  DESTROY_GRIB2_SH_V_ENCODER, MIOH=MIOH )
      ENDIF
      IF ( LGENABLE_GRIB2_SH_S ) THEN
        CALL ENCODERS( ENCODER_IDX( SURFACE_E,         REPRES_SPECTRAL_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_SH_S_ENCODER,  DESTROY_GRIB2_SH_S_ENCODER, MIOH=MIOH )
      ENDIF
      IF ( LGENABLE_GRIB2_SH_M ) THEN
        CALL ENCODERS( ENCODER_IDX( MODEL_LEVEL_E,     REPRES_SPECTRAL_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_SH_M_ENCODER,  DESTROY_GRIB2_SH_M_ENCODER, MIOH=MIOH )
      ENDIF

    ELSE

      IF ( LGENABLE_GRIB1_GG_P ) THEN
        CALL ENCODERS( ENCODER_IDX( PRESSURE_LEVEL_E,  REPRES_GRIDDED_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_GG_P_ENCODER,  DESTROY_GRIB1_GG_P_ENCODER )
      ENDIF
      IF ( LGENABLE_GRIB1_GG_T ) THEN
        CALL ENCODERS( ENCODER_IDX( THETA_LEVEL_E,     REPRES_GRIDDED_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_GG_T_ENCODER,  DESTROY_GRIB1_GG_T_ENCODER )
      ENDIF
      IF ( LGENABLE_GRIB1_GG_V ) THEN
        CALL ENCODERS( ENCODER_IDX( VORTICITY_LEVEL_E, REPRES_GRIDDED_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_GG_V_ENCODER,  DESTROY_GRIB1_GG_V_ENCODER )
      ENDIF
      IF ( LGENABLE_GRIB1_GG_S ) THEN
        CALL ENCODERS( ENCODER_IDX( SURFACE_E,         REPRES_GRIDDED_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_GG_S_ENCODER,  DESTROY_GRIB1_GG_S_ENCODER )
      ENDIF
      IF ( LGENABLE_GRIB1_GG_M ) THEN
        CALL ENCODERS( ENCODER_IDX( MODEL_LEVEL_E,     REPRES_GRIDDED_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_GG_M_ENCODER,  DESTROY_GRIB1_GG_M_ENCODER )
      ENDIF
      IF ( LGENABLE_GRIB1_GG_WI ) THEN
        CALL ENCODERS( ENCODER_IDX( WAVE_INT_E,        REPRES_GRIDDED_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_GG_WI_ENCODER, DESTROY_GRIB1_GG_WI_ENCODER )
      ENDIF
      IF ( LGENABLE_GRIB1_GG_WS ) THEN
        CALL ENCODERS( ENCODER_IDX( WAVE_SPEC_E,       REPRES_GRIDDED_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_GG_WS_ENCODER, DESTROY_GRIB1_GG_WS_ENCODER )
      ENDIF

      ! Grib1 spherical harmonics encoders
      IF ( LGENABLE_GRIB1_SH_P ) THEN
        CALL ENCODERS( ENCODER_IDX( PRESSURE_LEVEL_E,  REPRES_SPECTRAL_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_SH_P_ENCODER,  DESTROY_GRIB1_SH_P_ENCODER )
      ENDIF
      IF ( LGENABLE_GRIB1_SH_T ) THEN
        CALL ENCODERS( ENCODER_IDX( THETA_LEVEL_E,     REPRES_SPECTRAL_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_SH_T_ENCODER,  DESTROY_GRIB1_SH_T_ENCODER )
      ENDIF
      IF ( LGENABLE_GRIB1_SH_V ) THEN
        CALL ENCODERS( ENCODER_IDX( VORTICITY_LEVEL_E, REPRES_SPECTRAL_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_SH_V_ENCODER,  DESTROY_GRIB1_SH_V_ENCODER )
      ENDIF
      IF ( LGENABLE_GRIB1_SH_S ) THEN
        CALL ENCODERS( ENCODER_IDX( SURFACE_E,         REPRES_SPECTRAL_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_SH_S_ENCODER,  DESTROY_GRIB1_SH_S_ENCODER )
      ENDIF
      IF ( LGENABLE_GRIB1_SH_M ) THEN
        CALL ENCODERS( ENCODER_IDX( MODEL_LEVEL_E,     REPRES_SPECTRAL_E, GRIB1_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB1_SH_M_ENCODER,  DESTROY_GRIB1_SH_M_ENCODER )
      ENDIF

      ! Grib2 gridded encoders
      IF ( LGENABLE_GRIB2_GG_P ) THEN
        CALL ENCODERS( ENCODER_IDX( PRESSURE_LEVEL_E,  REPRES_GRIDDED_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_GG_P_ENCODER,  DESTROY_GRIB2_GG_P_ENCODER )
      ENDIF
      IF ( LGENABLE_GRIB2_GG_T ) THEN
        CALL ENCODERS( ENCODER_IDX( THETA_LEVEL_E,     REPRES_GRIDDED_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_GG_T_ENCODER,  DESTROY_GRIB2_GG_T_ENCODER )
      ENDIF
      IF ( LGENABLE_GRIB2_GG_V ) THEN
        CALL ENCODERS( ENCODER_IDX( VORTICITY_LEVEL_E, REPRES_GRIDDED_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_GG_V_ENCODER,  DESTROY_GRIB2_GG_V_ENCODER )
      ENDIF
      IF ( LGENABLE_GRIB2_GG_S ) THEN
        CALL ENCODERS( ENCODER_IDX( SURFACE_E,         REPRES_GRIDDED_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_GG_S_ENCODER,  DESTROY_GRIB2_GG_S_ENCODER )
      ENDIF
      IF ( LGENABLE_GRIB2_GG_M ) THEN
        CALL ENCODERS( ENCODER_IDX( MODEL_LEVEL_E,     REPRES_GRIDDED_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_GG_M_ENCODER,  DESTROY_GRIB2_GG_M_ENCODER )
      ENDIF
      IF ( LGENABLE_GRIB2_GG_WI ) THEN
        CALL ENCODERS( ENCODER_IDX( WAVE_INT_E,        REPRES_GRIDDED_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_GG_WI_ENCODER, DESTROY_GRIB2_GG_WI_ENCODER )
      ENDIF
      IF ( LGENABLE_GRIB2_GG_WS ) THEN
        CALL ENCODERS( ENCODER_IDX( WAVE_SPEC_E,       REPRES_GRIDDED_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_GG_WS_ENCODER, DESTROY_GRIB2_GG_WS_ENCODER )
      ENDIF

      ! Grib2 gridded encoders
      IF ( LGENABLE_GRIB2_SH_P ) THEN
        CALL ENCODERS( ENCODER_IDX( PRESSURE_LEVEL_E,  REPRES_SPECTRAL_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_SH_P_ENCODER,  DESTROY_GRIB2_SH_P_ENCODER )
      ENDIF
      IF ( LGENABLE_GRIB2_SH_T ) THEN
        CALL ENCODERS( ENCODER_IDX( THETA_LEVEL_E,     REPRES_SPECTRAL_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_SH_T_ENCODER,  DESTROY_GRIB2_SH_T_ENCODER )
      ENDIF
      IF ( LGENABLE_GRIB2_SH_V ) THEN
        CALL ENCODERS( ENCODER_IDX( VORTICITY_LEVEL_E, REPRES_SPECTRAL_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_SH_V_ENCODER,  DESTROY_GRIB2_SH_V_ENCODER )
      ENDIF
      IF ( LGENABLE_GRIB2_SH_S ) THEN
        CALL ENCODERS( ENCODER_IDX( SURFACE_E,         REPRES_SPECTRAL_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_SH_S_ENCODER,  DESTROY_GRIB2_SH_S_ENCODER )
      ENDIF
      IF ( LGENABLE_GRIB2_SH_M ) THEN
        CALL ENCODERS( ENCODER_IDX( MODEL_LEVEL_E,     REPRES_SPECTRAL_E, GRIB2_E ) )%CREATE( CFG, MODEL_PARAMS, METADATA_KIND, MAKE_GRIB2_SH_M_ENCODER,  DESTROY_GRIB2_SH_M_ENCODER )
      ENDIF
    ENDIF

  ENDIF

  ! Destroy ERRMSG
  IF ( ALLOCATED(ERRMSG) ) THEN
    DEALLOCATE(ERRMSG)
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate ancoders:'//TRIM(ADJUSTL(ERRMSG)) )
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

END SUBROUTINE MAKE_ENCODERS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





!>
!> @brief Destroy the encoder's table
!>
!> This subroutine is responsible for finlising and destroyng the encoder's table.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'DESTROY_ENCODERS'
SUBROUTINE DESTROY_ENCODERS( )
  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Initialisation of utils
  USE :: ENCODERS_UTILS_MOD, ONLY: UTILS_FREE

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Destroy all the encoders
  IF ( ALLOCATED(ENCODERS) ) THEN

    ! Free all the memory used by utils
    CALL UTILS_FREE()

    ! Call the finalization procedures
    DO I = 1, SIZE(ENCODERS,1)
        CALL ENCODERS(I)%DEALLOCATE( )
    ENDDO

    ! Free memory
    DEALLOCATE(ENCODERS)

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE DESTROY_ENCODERS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Create the an encoder
!>
!> This function is responsible for creating and initializing an encoder
!>
!> @param [inout] this           Instance of the container
!> @param [in]    PROCESSOR_TOPO Processor topology to be used in a multiprocessor run
!> @param [in]    MODEL_PARAMS   Model parameters that are frozen during the simulation
!> @param [in]    make_fcn       Constructor of an encoder
!> @param [in]    destroy_fcn    Destructor of an encoder
!> @param [in]    ctype          Type of the metadata to be used internally [grib|multio]
!> @param [in]    optionalData   Optional data used as additional argument to initialize metadata
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'CONTAINER_CREATE'
SUBROUTINE CONTAINER_CREATE( THIS, CFG, MODEL_PARAMS, METADATA_KIND, MAKE_FCN, DESTROY_FCN, MIOH )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION
  USE :: MULTIO_API,                 ONLY: MULTIO_HANDLE

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS


IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_ENCODER_CONTAINER_T), INTENT(INOUT) :: THIS
  TYPE(FCKIT_CONFIGURATION),       INTENT(IN)    :: CFG
  TYPE(MODEL_PAR_T), TARGET,       INTENT(IN)    :: MODEL_PARAMS
  CHARACTER(LEN=*),                INTENT(IN)    :: METADATA_KIND
  PROCEDURE(MAKE_FCN_IF)                         :: MAKE_FCN
  PROCEDURE(DESTROY_FCN_IF)                      :: DESTROY_FCN
  TYPE(MULTIO_HANDLE), OPTIONAL,   INTENT(IN)    :: MIOH

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Associate the encoder
  IF ( .NOT. ASSOCIATED(THIS%ENCODER_) ) THEN
    THIS%MAKE_FCN_    => MAKE_FCN
    THIS%DESTROY_FCN_ => DESTROY_FCN
    IF ( PRESENT(MIOH) ) THEN
      CALL THIS%MAKE_FCN_( CFG, MODEL_PARAMS, METADATA_KIND, THIS%ENCODER_, MIOH=MIOH )
    ELSE
      CALL THIS%MAKE_FCN_( CFG, MODEL_PARAMS, METADATA_KIND, THIS%ENCODER_ )
    ENDIF
  ELSE
    PP_DEBUG_CRITICAL_THROW( 1 )
  ENDIF

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Encoder already associated' )
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

END SUBROUTINE CONTAINER_CREATE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_ATM'
FUNCTION ENCODE_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, MSG, METADATA ) RESULT(EX)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,              ONLY: JPIB_K
  USE :: OM_CORE_MOD,              ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,              ONLY: OM_ATM_MSG_T
  USE :: OM_CORE_MOD,              ONLY: CURR_TIME_T
  USE :: GRIB_INFO_DATA_MOD,       ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,           ONLY: TIME_HISTORY_T
  USE :: TIME_RUNTIME_UTILS_MOD,   ONLY: COMPUTE_CURRENT_TIME
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: GRIB_ENCODER_INDEXER_MOD, ONLY: ENCODER_IDX
!!  USE :: ENCODERS_UTILS_MOD,       ONLY: TIME_IS_VALID

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Function result
  LOGICAL :: EX

  ! Local variables
  INTEGER(KIND=JPIB_K) :: IDX
  TYPE(CURR_TIME_T) :: CURR_TIME

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(ENCODERS), 1 )

  IF ( COMPUTE_CURRENT_TIME( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME ) ) THEN

    ! Get the encoder index
    IDX = ENCODER_IDX( GRIB_INFO, MSG )

    ! Call the encoder
    EX = ENCODERS(IDX)%ENCODE_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA )

  ELSE
    EX = .FALSE.
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Encoders not allocated' )
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

END FUNCTION ENCODE_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_WAM'
FUNCTION ENCODE_WAM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, MSG, METADATA ) RESULT(EX)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,              ONLY: JPIB_K
  USE :: OM_CORE_MOD,              ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,              ONLY: OM_WAM_MSG_T
  USE :: OM_CORE_MOD,              ONLY: CURR_TIME_T
  USE :: GRIB_INFO_DATA_MOD,       ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,           ONLY: TIME_HISTORY_T
  USE :: TIME_RUNTIME_UTILS_MOD,   ONLY: COMPUTE_CURRENT_TIME
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: GRIB_ENCODER_INDEXER_MOD, ONLY: ENCODER_IDX
!!  USE :: ENCODERS_UTILS_MOD,       ONLY: TIME_IS_VALID

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(OM_WAM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Function result
  LOGICAL :: EX

  ! Local variables
  INTEGER(KIND=JPIB_K) :: IDX
  TYPE(CURR_TIME_T) :: CURR_TIME

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(ENCODERS), 1 )

  IF ( COMPUTE_CURRENT_TIME( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME ) ) THEN

    ! Get the encoder index
    IDX = ENCODER_IDX( GRIB_INFO, MSG )

    ! Call the encoder
    EX = ENCODERS(IDX)%ENCODE_WAM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA )

  ELSE
    EX = .FALSE.
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Encoder not allocated' )
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

END FUNCTION ENCODE_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



!>
!> @brief Encode a field
!>
!> This function is used to encode a field
!>
!> @param [inout] this           Instance of the container
!> @param [inout] PROCESSOR_TOPO Information related to the topology of
!>                               the processors involved in the computation
!> @param [inout] MODEL_PARAMS   Parameters of the model
!> @param [inout] GRIB_INFO      Grib information about the field to be encoded
!> @param [inout] MSG            All the informations specifica to the current field
!> @param [inout] METADATA       Handle to be modified by the encoder
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CONTAINER_ENCODE_ATM'
FUNCTION CONTAINER_ENCODE_ATM( THIS, MODEL_PARAMS, GRIB_INFO, &
& TIME_HIST, CURR_TIME, MSG, METADATA ) RESULT(EX)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,         ONLY: JPIB_K
  USE :: OM_CORE_MOD,         ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,         ONLY: OM_ATM_MSG_T
  USE :: OM_CORE_MOD,         ONLY: CURR_TIME_T
  USE :: TRACK_TIME_MOD,      ONLY: TIME_HISTORY_T
  USE :: GRIB_INFO_DATA_MOD,  ONLY: GRIB_INFO_T
  USE :: METADATA_BASE_MOD,   ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_ENCODER_CONTAINER_T), INTENT(INOUT) :: THIS
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Function result
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Encode the field
  IF ( ASSOCIATED(THIS%ENCODER_) ) THEN
    EX = THIS%ENCODER_%ENCODE_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, &
&                                  CURR_TIME, MSG, METADATA )
  ELSE
    !> @todo
    !> If encoder is not associated then pass to the next message.
    !> This is a temporary hack!!!
    EX = .FALSE.
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Encoder not associated' )
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

END FUNCTION CONTAINER_ENCODE_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE






!>
!> @brief Encode a field
!>
!> This function is used to encode a field
!>
!> @param [inout] this           Instance of the container
!> @param [inout] PROCESSOR_TOPO Information related to the topology of
!>                               the processors involved in the computation
!> @param [inout] MODEL_PARAMS   Parameters of the model
!> @param [inout] GRIB_INFO      Grib information about the field to be encoded
!> @param [inout] MSG            All the informations specifica to the current field
!> @param [inout] METADATA       Handle to be modified by the encoder
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CONTAINER_ENCODE_WAM'
FUNCTION CONTAINER_ENCODE_WAM( THIS, MODEL_PARAMS, GRIB_INFO, &
& TIME_HIST, CURR_TIME, MSG, METADATA ) RESULT(EX)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,         ONLY: JPIB_K
  USE :: OM_CORE_MOD,         ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,         ONLY: OM_WAM_MSG_T
  USE :: OM_CORE_MOD,         ONLY: CURR_TIME_T
  USE :: TRACK_TIME_MOD,      ONLY: TIME_HISTORY_T
  USE :: GRIB_INFO_DATA_MOD,  ONLY: GRIB_INFO_T
  USE :: METADATA_BASE_MOD,   ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_ENCODER_CONTAINER_T), INTENT(INOUT) :: THIS
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_WAM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Function result
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Encode the field
  IF ( ASSOCIATED(THIS%ENCODER_) ) THEN
    EX = THIS%ENCODER_%ENCODE_WAM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, &
&                                  CURR_TIME, MSG, METADATA )
  ELSE
    !> @todo
    !> If encoder is not associated then pass to the next message.
    !> This is a temporary hack!!!
    EX = .FALSE.
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Encoder not associated' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END FUNCTION CONTAINER_ENCODE_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Encode a field
!>
!> This function is used to destroy an instance of the encoder
!>
!> @param [inout] this         Instance of the container
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'CONTAINER_DEALLOCATE'
SUBROUTINE CONTAINER_DEALLOCATE( THIS )

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_ENCODER_CONTAINER_T), INTENT(INOUT) :: THIS

  ! Local variables
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Encode the field
  IF ( ASSOCIATED(THIS%ENCODER_) ) THEN
    CALL THIS%DESTROY_FCN_( THIS%ENCODER_ )
    NULLIFY(THIS%MAKE_FCN_)
    NULLIFY(THIS%ENCODER_)
    NULLIFY(THIS%DESTROY_FCN_)
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE CONTAINER_DEALLOCATE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE GRIB_ENCODER_MANAGER_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
