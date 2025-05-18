!>
!> @file grib2_section3_040_mod.F90
!>
!> @brief Module for managing GRIB2 Section 3 operations.
!>
!> The `GRIB2_SECTION3_040_MOD` module contains procedures to initialize, allocate,
!> preset, run, and clean up the resources associated with GRIB2 Section 3 objects.
!> This module provides thread-safe operations and includes extensive use of debugging,
!> logging, and tracing capabilities, making it robust for production and testing.
!>
!> The key operations covered by this module include:
!>   - Initialization of GRIB2 Section 3 objects.
!>   - Allocation of resources.
!>   - Presetting internal parameters.
!>   - Managing runtime operations based on input parameters.
!>   - Cleaning up and deallocating resources after use.
!>
!> @section interface
!>
!> The module exports the following procedures:
!>   - @see GRIB2_SECTION3_040_INIT
!>   - @see GRIB2_SECTION3_040_ALLOCATE
!>   - @see GRIB2_SECTION3_040_PRESET
!>   - @see GRIB2_SECTION3_040_RUNTIME
!>   - @see GRIB2_SECTION3_040_TO_BE_ENCODED
!>   - @see GRIB2_SECTION3_040_FREE
!>
!> @section dependencies
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MESSAGE_T
!>   - @dependency [TYPE] METADATA_BASE_MOD::METADATA_BASE_A
!>   - @dependency [TYPE] OM_CORE_MOD::TIME_HISTORY_T
!>   - @dependency [TYPE] OM_CORE_MOD::CURR_TIME_T
!>
!> @subsection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @author Mirco Valentini
!> @date   August, 2024
!>

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'grib2_section3_040_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB2_SECTION3_040_MOD'
MODULE GRIB2_SECTION3_040_MOD

  !> Symbols imported from other modules within the project.
  USE :: GRIB_SECTION_BASE_MOD, ONLY: GRIB_SECTION_BASE_A

IMPLICIT NONE

!>
!> Default symbols visibility
PRIVATE

!>
!> @brief Type definition for GRIB2 Section 3 handler.
!>
!> The `GRIB2_SECTION3_040_T` type extends the base class `GRIB_SECTION_BASE_A` and
!> provides concrete implementations of initialization, allocation, preset, runtime,
!> encoding checks, and cleanup operations for GRIB2 Section 3 objects.
!>
!> This type ensures that the required resources are properly managed through thread-safe,
!> non-overridable methods, providing robustness in both multi-threaded and single-threaded
!> environments.
!>
TYPE, EXTENDS(GRIB_SECTION_BASE_A) :: GRIB2_SECTION3_040_T

  !> Default symbols visibility
  PRIVATE

CONTAINS

  !>
  !> @brief Initializes the GRIB2 Section 3 object.
  !>
  !> This procedure sets up the necessary parameters and prepares the
  !> object for use.
  !> The procedure starts from a yaml configuration file to construct the
  !> GRIB2 encoder.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: INIT => GRIB2_SECTION3_040_INIT

  !>
  !> @brief Allocates resources for the GRIB2 Section 3 object.
  !>
  !> This procedure allocates memory and other necessary resources for
  !> the object based on provided parameters.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: ALLOCATE => GRIB2_SECTION3_040_ALLOCATE

  !>
  !> @brief Presets the parameters of the GRIB2 Section 3 object.
  !>
  !> This procedure configures the internal parameters of the object
  !> before runtime execution.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: PRESET => GRIB2_SECTION3_040_PRESET

  !>
  !> @brief Manages the runtime execution of GRIB2 Section 3 operations.
  !>
  !> This procedure handles operations and computations during runtime,
  !> making use of time and metadata information.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: RUNTIME => GRIB2_SECTION3_040_RUNTIME

  !>
  !> @brief Frees resources allocated for the GRIB2 Section 3 object.
  !>
  !> This procedure deallocates resources and performs cleanup after
  !> the object has been used.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: FREE => GRIB2_SECTION3_040_FREE

END TYPE


!>
!> Public symbols (dataTypes)
PUBLIC :: GRIB2_SECTION3_040_T

CONTAINS

!>
!> @brief Initializes GRIB2 Section 3 for a given object using the provided parameters.
!>
!> This function initializes a GRIB2 Section 3 object (`THIS`) using the provided model parameters (`PARAMS`)
!> and configuration data (`CFG`). The process can be run in verbose mode if specified. The function
!> is thread-safe and returns an error code indicating the success or failure of the operation.
!>
!> @section interface
!>   @param [inout] THIS  An object of type `GRIB2_SECTION3_040_T` representing the GRIB section being initialized.
!>   @param [in]    CFG   The YAML configuration object of type `YAML_CONFIGURATION_T`.
!>   @param [in]    OPT   The encoder options structure of type `ENCODER_OPTIONS_T`.
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
!> @see GRIB2_SECTION3_040_INIT
!> @see GRIB2_SECTION3_040_ALLOCATE
!> @see GRIB2_SECTION3_040_PRESET
!> @see GRIB2_SECTION3_040_RUNTIME
!> @see GRIB2_SECTION3_040_TO_BE_ENCODED
!> @see GRIB2_SECTION3_040_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION3_040_INIT'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION3_040_INIT( THIS, &
&               CFG, OPT, FACTORIES, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_T
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_FACTORY_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION3_040_T),  INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(GRIB_SECTION_FACTORY_T), INTENT(IN)    :: FACTORIES
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

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

  ! Initialise the section
  THIS%TYPE_ = 'SECTION'
  THIS%SUBTYPE_ = 'GRID_DEFINITION_SECTION'
  THIS%KIND_   = '3.40'

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

END FUNCTION GRIB2_SECTION3_040_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Allocates resources for GRIB2 Section 3 using the provided parameters.
!>
!> This function allocates resources for a GRIB2 Section 3 object (`THIS`) using the provided model parameters (`PARAMS`),
!> message structure (`MSG`), and metadata (`METADATA`). The process can be run in verbose mode if specified.
!> The function is thread-safe and returns an error code indicating the success or failure of the allocation process.
!>
!> @section interface
!>   @param [in]    THIS     An object of type `GRIB2_SECTION3_040_T` representing the GRIB section to allocate resources for.
!>   @param [in]    MSG      All the mars keywords needed to describe the field `FORTRAN_MESSAGE_T`.
!>   @param [in]    PAR      All information outside mars keywords needed to describe the field `PARAMETRIZATION_T`.
!>   @param [in]    OPT      The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] METADATA A pointer to the metadata object of type `METADATA_BASE_A` used during allocation.
!>   @param [inout] HOOKS    A structure of type `HOOKS_T` that contains hooks for the allocation process.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MESSAGE_T
!>   - @dependency [TYPE] METADATA_BASE_MOD::METADATA_BASE_A
!>
!> @subsection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see GRIB2_SECTION3_040_ALLOCATE
!> @see GRIB2_SECTION3_040_INIT
!> @see GRIB2_SECTION3_040_PRESET
!> @see GRIB2_SECTION3_040_RUNTIME
!> @see GRIB2_SECTION3_040_TO_BE_ENCODED
!> @see GRIB2_SECTION3_040_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION3_040_ALLOCATE'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION3_040_ALLOCATE( THIS, &
&  MSG, PAR, OPT,  METADATA, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: REPRESENTATIONS_MOD,      ONLY: REDUCED_GG_T
  USE :: REPRESENTATIONS_MOD,      ONLY: REGULAR_GG_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION3_040_T),     INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: IDX
  INTEGER(KIND=JPIB_K) :: PARAM_SZ
  INTEGER(KIND=JPIB_K) :: EXPECTED_SZ

  !> Local variables
  INTEGER(KIND=JPIB_K) :: NPTS

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GEOMETRY_DESCRIPTION_NOT_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GEOMETRY_PL_ARRAY_NOT_ASSOCIATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_IMPLEMENTED=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GEOMETRY_PL_ARRAY_WRONG_SIZE=6_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA, ERRFLAG_METADATA )

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW(  .NOT. ASSOCIATED(METADATA), ERRFLAG_METADATA_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW(  .NOT. ASSOCIATED(PAR%GEOMETRY%REPRES), ERRFLAG_GEOMETRY_DESCRIPTION_NOT_ASSOCIATED )

  ! Enable section 3
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'gridDefinitionTemplateNumber', 40 )

  ! Configure the representation
  SELECT TYPE ( R => PAR%GEOMETRY%REPRES )

  CLASS IS (REGULAR_GG_T)


    ! Set the specific metadata for a regular_gg grid
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'gridType','regular_gg' )
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'truncateDegrees', R%TRUNCATE_DEGREES )
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'numberOfPointsAlongAMeridian', R%NUMBER_OF_POINTS_ALONG_A_MERIDIAN )
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'numberOfPointsAlongAParallel', R%NUMBER_OF_POINTS_ALONG_A_PARALLEL )
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'latitudeOfFirstGridPointInDegrees', R%LAT_FIRST_GP_DEG )
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'longitudeOfFirstGridPointInDegrees', R%LON_FIRST_GP_DEG )
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'latitudeOfLastGridPointInDegrees', R%LAT_LAST_GP_DEG )
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'longitudeOfLastGridPointInDegrees', R%LON_LAST_GP_DEG )
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'numberOfParallelsBetweenAPoleAndTheEquator', R%NUMBER_OF_PARALLELS_BETWEEN_POLE_AND_EQUATOR )
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'iDirectionIncrementInDegrees', R%IDIR_INC )

  CLASS IS (REDUCED_GG_T)

    ! Error handling
    PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(R%PL), ERRFLAG_GEOMETRY_PL_ARRAY_NOT_ASSOCIATED )
    PARAM_SZ = SIZE(R%PL)/2
    EXPECTED_SZ = R%NUMBER_OF_PARALLELS_BETWEEN_POLE_AND_EQUATOR
    PP_DEBUG_CRITICAL_COND_THROW( EXPECTED_SZ.NE.PARAM_SZ, ERRFLAG_GEOMETRY_PL_ARRAY_WRONG_SIZE )

    ! Set the specific metadata for a reduced_gg grid
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'gridType','reduced_gg' )
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'interpretationOfNumberOfPoints', 1_JPIB_K )
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'truncateDegrees', R%TRUNCATE_DEGREES )
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'numberOfPointsAlongAMeridian', R%NUMBER_OF_POINTS_ALONG_A_MERIDIAN )
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'latitudeOfFirstGridPointInDegrees', R%LAT_FIRST_GP_DEG )
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'longitudeOfFirstGridPointInDegrees', R%LON_FIRST_GP_DEG )
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'latitudeOfLastGridPointInDegrees', R%LAT_LAST_GP_DEG )
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'longitudeOfLastGridPointInDegrees', R%LON_LAST_GP_DEG )
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'numberOfParallelsBetweenAPoleAndTheEquator', R%NUMBER_OF_PARALLELS_BETWEEN_POLE_AND_EQUATOR )
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'pl', R%PL )
    PP_METADATA_SET_MISSING( METADATA, ERRFLAG_METADATA, 'iDirectionIncrement' )

  CLASS DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NOT_IMPLEMENTED )

  END SELECT

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA, ERRFLAG_METADATA )
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

    CHARACTER(LEN=32) :: CTMP1
    CHARACTER(LEN=32) :: CTMP2

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_METADATA_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'metadata not associated' )
    CASE (ERRFLAG_GEOMETRY_DESCRIPTION_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'geometry description not associated' )
    CASE (ERRFLAG_GEOMETRY_PL_ARRAY_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'geometry pl array not associated' )
    CASE (ERRFLAG_NOT_IMPLEMENTED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'not implemented' )
    CASE (ERRFLAG_METADATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'metadata error' )
    CASE (ERRFLAG_GEOMETRY_PL_ARRAY_WRONG_SIZE)
      CTMP1 = REPEAT(' ',32)
      WRITE(CTMP1,'(I32)') PARAM_SZ
      CTMP2 = REPEAT(' ',32)
      WRITE(CTMP2,'(I32)') EXPECTED_SZ
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong size of PL array or N has a invalid number' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Param size: '//TRIM(ADJUSTL(CTMP1)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Expected size: '//TRIM(ADJUSTL(CTMP2)) )
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

END FUNCTION GRIB2_SECTION3_040_ALLOCATE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Presets GRIB2 Section 3 using the provided parameters and message data.
!>
!> This function presets a GRIB2 Section 3 object (`THIS`) using the provided model parameters (`PARAMS`),
!> message structure (`MSG`), and metadata (`METADATA`). The process can be run in verbose mode if specified.
!> The function is thread-safe and returns an error code indicating the success or failure of the preset operation.
!>
!> @section interface
!>   @param [in]    THIS     An object of type `GRIB2_SECTION3_040_T` representing the GRIB section to be preset.
!>   @param [in]    MSG      The message object of type `FORTRAN_MESSAGE_T` used to handle preset-related messaging.
!>   @param [in]    PAR      The parametrization structure of type `PARAMETRIZATION_T` used for the preset operation.
!>   @param [in]    OPT      The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] METADATA A pointer to the metadata object of type `METADATA_BASE_A` used for presetting the section.
!>   @param [inout] HOOKS    A structure of type `HOOKS_T` that contains hooks for the preset operation.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MESSAGE_T
!>   - @dependency [TYPE] METADATA_BASE_MOD::METADATA_BASE_A
!>
!> @subsection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see GRIB2_SECTION3_040_PRESET
!> @see GRIB2_SECTION3_040_ALLOCATE
!> @see GRIB2_SECTION3_040_INIT
!> @see GRIB2_SECTION3_040_RUNTIME
!> @see GRIB2_SECTION3_040_TO_BE_ENCODED
!> @see GRIB2_SECTION3_040_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION3_040_PRESET'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION3_040_PRESET( THIS, &
&  MSG, PAR, OPT,  METADATA, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPRD_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION3_040_T),     INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: IDX
  INTEGER(KIND=JPIB_K) :: NPTS
  INTEGER(KIND=JPIB_K) :: NLAT
  INTEGER(KIND=JPIB_K) :: NLAT_BETWEEN_POLE_AND_EQUATOR
  REAL(KIND=JPRD_K) :: LAT_FIRST_GP_DEG
  REAL(KIND=JPRD_K) :: LAT_LAST_GP_DEG
  REAL(KIND=JPRD_K) :: LON_FIRST_GP_DEG
  REAL(KIND=JPRD_K) :: LON_LAST_GP_DEG

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GEOMETRY_DESCRIPTION_NOT_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GEOMETRY_DESCRIPTION_OUT_OF_BOUNDS=3_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA, ERRFLAG_METADATA )

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

#if 0
  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(METADATA), ERRFLAG_METADATA )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(PAR%GEO%GG), ERRFLAG_GEOMETRY_DESCRIPTION_NOT_ASSOCIATED )

  ! Initialize the index (In general this index should com from a search procedure that uses the MARS
  ! keyword "grid" to look for the index of the proper gg grid inside the parametrization)
  IDX = 1

  ! Search the index of the gg grid inside the parametrization. MSG%GRID is the mars keyword "grid" i.e. grid='O400'
  ! PP_TRYCALL(ERRFLAG_SEARCH_GRID_DEFINITION_INDEX) PAR%GEO%GG%SEARCH_GRID_DEFINITION_INDEX( IDX, MSG%GRID, HOOKS )

  ! Check that the index is correct
  PP_DEBUG_CRITICAL_COND_THROW( IDX < 1 .OR. IDX > SIZE(PAR%GEO%GG), ERRFLAG_GEOMETRY_DESCRIPTION_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( IDX < 1 .OR. IDX > SIZE(PAR%GEO%GG), ERRFLAG_GEOMETRY_DESCRIPTION_OUT_OF_BOUNDS )

  ! Extract the number of points and the number of latitudes
  NPTS = PAR%GEO%GG(IDX)%NPTS ! Sum of the pl array
  NLAT = PAR%GEO%GG(IDX)%NLAT
  NLAT_BETWEEN_POLE_AND_EQUATOR = NLAT/2

  LAT_FIRST_GP_DEG=180.0_JPRD_K/(2.0_JPRD_K*ASIN(1.0_JPRD_K))*PAR%GEO%GG(IDX)%NORTH
  LAT_LAST_GP_DEG=180.0_JPRD_K/(2.0_JPRD_K*ASIN(1.0_JPRD_K))*PAR%GEO%GG(IDX)%SOUTH
  LON_FIRST_GP_DEG=0.0_JPRD_K
  LON_LAST_GP_DEG=360.0_JPRD_K-360.0_JPRD_K/REAL(PAR%GEO%GG(IDX)%NLON,JPRD_K)

  ! Preset Earth geometry
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA,  'shapeOfTheEarth', 6 )
  PP_METADATA_SET_MISSING( METADATA, ERRFLAG_METADATA, 'scaleFactorOfRadiusOfSphericalEarth' )
  PP_METADATA_SET_MISSING( METADATA, ERRFLAG_METADATA, 'scaledValueOfRadiusOfSphericalEarth' )
  PP_METADATA_SET_MISSING( METADATA, ERRFLAG_METADATA, 'scaleFactorOfEarthMajorAxis' )
  PP_METADATA_SET_MISSING( METADATA, ERRFLAG_METADATA, 'scaledValueOfEarthMajorAxis' )
  PP_METADATA_SET_MISSING( METADATA, ERRFLAG_METADATA, 'scaleFactorOfEarthMinorAxis' )
  PP_METADATA_SET_MISSING( METADATA, ERRFLAG_METADATA, 'scaledValueOfEarthMinorAxis' )

  ! Preset grid size
  PP_METADATA_SET_MISSING( METADATA, ERRFLAG_METADATA, 'Ni' )
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA,  'Nj', NLAT ) ! numberOfPointsAlongAMeridian

  ! Preset grid resolution
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'basicAngleOfTheInitialProductionDomain', 0 )
  PP_METADATA_SET_MISSING( METADATA, ERRFLAG_METADATA, 'subdivisionsOfBasicAngle' )


  ! Preset grid spacing
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA,  'latitudeOfFirstGridPointInDegrees', LAT_FIRST_GP_DEG )
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA,  'longitudeOfFirstGridPointInDegrees', LON_FIRST_GP_DEG )

  PP_METADATA_SET( METADATA, ERRFLAG_METADATA,  'resolutionAndComponentFlags', 0 )

  PP_METADATA_SET( METADATA, ERRFLAG_METADATA,  'latitudeOfLastGridPointInDegrees', LAT_LAST_GP_DEG )
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA,  'longitudeOfLastGridPointInDegrees', LON_LAST_GP_DEG )

  PP_METADATA_SET_MISSING( METADATA, ERRFLAG_METADATA, 'iDirectionIncrement' )
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA,  'N', NLAT_BETWEEN_POLE_AND_EQUATOR ) ! numberOfParallelsBetweenAPoleAndTheEquator

  ! Reset the pl array another time just to be sure (in any case it is done at the beginning of the simulation)
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'scanningMode', 0 )
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'pl', PAR%GEO%GG(IDX)%PL )
#endif
  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA, ERRFLAG_METADATA )
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
    CASE ( ERRFLAG_METADATA )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error using metadata' )
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

END FUNCTION GRIB2_SECTION3_040_PRESET
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Executes runtime processing for GRIB2 Section 3 using provided parameters, message data, and time history.
!>
!> This function performs runtime operations for a GRIB2 Section 3 object (`THIS`) using the provided model parameters (`PARAMS`),
!> message structure (`MSG`), current time (`CURR_TIME`), time history (`TIME_HISTORY`), and metadata (`METADATA`).
!> The process can be run in verbose mode if specified. The function is thread-safe and returns an error code indicating
!> the success or failure of the runtime operation.
!>
!> @section interface
!>   @param [in]    THIS      An object of type `GRIB2_SECTION3_040_T` representing the GRIB section for runtime execution.
!>   @param [in]    MSG       The message object of type `FORTRAN_MESSAGE_T` used to handle preset-related messaging.
!>   @param [in]    PAR       The parametrization structure of type `PARAMETRIZATION_T` used for the preset operation.
!>   @param [in]    TIME_HIST The time history object of type `TIME_HISTORY_T` providing historical time data.
!>   @param [in]    CURR_TIME The current time object of type `CURR_TIME_T` for the runtime phase.
!>   @param [in]    OPT       The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] METADATA  A pointer to the metadata object of type `METADATA_BASE_A` used during runtime.
!>   @param [inout] HOOKS     A structure of type `HOOKS_T` that contains hooks for runtime operations.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MESSAGE_T
!>   - @dependency [TYPE] METADATA_BASE_MOD::METADATA_BASE_A
!>   - @dependency [TYPE] OM_CORE_MOD::TIME_HISTORY_T
!>   - @dependency [TYPE] OM_CORE_MOD::CURR_TIME_T
!>
!> @subsection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see GRIB2_SECTION3_040_RUNTIME
!> @see GRIB2_SECTION3_040_ALLOCATE
!> @see GRIB2_SECTION3_040_INIT
!> @see GRIB2_SECTION3_040_PRESET
!> @see GRIB2_SECTION3_040_TO_BE_ENCODED
!> @see GRIB2_SECTION3_040_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION3_040_RUNTIME'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION3_040_RUNTIME( THIS, &
&  MSG, PAR, OPT, METADATA, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION3_040_T),     INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA=1_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA, ERRFLAG_METADATA )

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(METADATA), ERRFLAG_METADATA )

  ! Nothing to do for section 3 runtime

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA, ERRFLAG_METADATA )
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
    CASE ( ERRFLAG_METADATA )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error using metadata' )
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

END FUNCTION GRIB2_SECTION3_040_RUNTIME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Frees resources associated with GRIB2 Section 3 object.
!>
!> This function deallocates and cleans up resources associated with the GRIB2 Section 3 object (`THIS`).
!> The process can be run in verbose mode for additional output. The function is thread-safe and returns an
!> error code indicating the success or failure of the operation.
!>
!> @section interface
!>   @param [inout] THIS  An object of type `GRIB2_SECTION3_040_T` representing the GRIB section to be freed.
!>   @param [in]    OPT   The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies
!>
!> @subsection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see GRIB2_SECTION3_040_INIT
!> @see GRIB2_SECTION3_040_ALLOCATE
!> @see GRIB2_SECTION3_040_PRESET
!> @see GRIB2_SECTION3_040_RUNTIME
!> @see GRIB2_SECTION3_040_TO_BE_ENCODED
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION3_040_FREE'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION3_040_FREE( THIS, OPT, DESTRUCTORS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_DESTRUCTOR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION3_040_T),     INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  TYPE(GRIB_SECTION_DESTRUCTOR_T), INTENT(IN)    :: DESTRUCTORS
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

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

END FUNCTION GRIB2_SECTION3_040_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE GRIB2_SECTION3_040_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
