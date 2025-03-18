!>
!> @file grib2_section4_factory_mod.F90
!>
!> @brief Module containing the factory function for creating or initializing GRIB2 Section 4 objects.
!>
!> The `GRIB2_SECTION4_FACTORY_MOD` provides a factory function that creates or initializes
!> instances of GRIB2 Section 4 objects. The function relies on various data structures and
!> types defined within the model's core and data types modules, as well as a YAML configuration
!> for initializing the section's parameters. Debugging, logging, and tracing features are enabled
!> via preprocessor directives to allow additional output when needed.
!>
!> @section local dependencies
!>   - @dependency [PARAMETER] OM_CORE_MOD::JPIB_K
!>   - @dependency [TYPE] GRIB2_SECTION4_000_MOD::GRIB2_SECTION4_000_T
!>   - @dependency [TYPE] GRIB2_SECTION4_001_MOD::GRIB2_SECTION4_001_T
!>   - @dependency [TYPE] GRIB2_SECTION4_008_MOD::GRIB2_SECTION4_008_T
!>   - @dependency [TYPE] GRIB2_SECTION4_011_MOD::GRIB2_SECTION4_011_T
!>   - @dependency [TYPE] GRIB2_SECTION4_032_MOD::GRIB2_SECTION4_032_T
!>   - @dependency [TYPE] GRIB2_SECTION4_040_MOD::GRIB2_SECTION4_040_T
!>   - @dependency [TYPE] GRIB2_SECTION4_041_MOD::GRIB2_SECTION4_041_T
!>   - @dependency [TYPE] GRIB2_SECTION4_042_MOD::GRIB2_SECTION4_042_T
!>   - @dependency [TYPE] GRIB2_SECTION4_043_MOD::GRIB2_SECTION4_043_T
!>   - @dependency [TYPE] GRIB2_SECTION4_099_MOD::GRIB2_SECTION4_099_T
!>   - @dependency [TYPE] GRIB2_SECTION4_103_MOD::GRIB2_SECTION4_103_T
!>   - @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>
!> @section special dependencies
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


#define PP_FILE_NAME 'grib2_section4_factory_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB2_SECTION4_FACTORY_MOD'
MODULE GRIB2_SECTION4_FACTORY_MOD

IMPLICIT NONE

!>
!> Default symbols visibility
PRIVATE

!>
!> Key to be used to recover the section4 number from the YAML configuration.
CHARACTER(LEN=*), PARAMETER :: SECTION4_KEY='template-number'


!>
!> Generic interface to the make encoder function.
INTERFACE MAKE_GRIB2_SECTION4
  MODULE PROCEDURE MAKE_GRIB2_SECTION4_CFG
  MODULE PROCEDURE MAKE_GRIB2_SECTION4_LAZY
END INTERFACE


!>
!> Public symbols (dataTypes)
PUBLIC :: MAKE_GRIB2_SECTION4
PUBLIC :: DESTROY_GRIB2_SECTION4

CONTAINS

!>
!> @brief Factory function for creating or initializing GRIB2 Section 4 objects.
!>
!> This function acts as a factory for creating or initializing a GRIB2 Section 4 object
!> based on the provided parameters. It assigns the proper type (`GRIB2_SECTION4_000_T`)
!> to the `GRIB2_SECTION4` object and configures it using the provided model parameters,
!> ID, and YAML configuration. If verbose mode is enabled, additional debug information
!> is output during the process.
!>
!> @param [inout] GRIB2_SECTION4 The GRIB2 Section 4 object that will be created or initialized.
!>                              It must be a pointer of type `GRIB_SECTION_BASE_A`.
!> @param [in]    CFG YAML configuration object used to configure the GRIB2 Section 4 object.
!> @param [in]    OPT The encoder options structure of type `ENCODER_OPTIONS_T`.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Section that can be constructed with this factory
!>   - `GRIB2_SECTION4_000_T`
!>   - `GRIB2_SECTION4_001_T`
!>   - `GRIB2_SECTION4_008_T`
!>   - `GRIB2_SECTION4_011_T`
!>   - `GRIB2_SECTION4_032_T`
!>   - `GRIB2_SECTION4_040_T`
!>   - `GRIB2_SECTION4_041_T`
!>   - `GRIB2_SECTION4_042_T`
!>   - `GRIB2_SECTION4_043_T`
!>   - `GRIB2_SECTION4_045_T`
!>   - `GRIB2_SECTION4_046_T`
!>   - `GRIB2_SECTION4_048_T`
!>   - `GRIB2_SECTION4_049_T`
!>   - `GRIB2_SECTION4_050_T`
!>   - `GRIB2_SECTION4_086_T`
!>   - `GRIB2_SECTION4_087_T`
!>   - `GRIB2_SECTION4_099_T`
!>   - `GRIB2_SECTION4_100_T`
!>   - `GRIB2_SECTION4_103_T`
!>   - `GRIB2_SECTION4_104_T`
!>   - `GRIB2_SECTION4_105_T`
!>   - `GRIB2_SECTION4_107_T`
!>   - `GRIB2_SECTION4_108_T`
!>   - `GRIB2_SECTION4_109_T`
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] OM_CORE_MOD::JPIB_K
!>   - @dependency [TYPE] GRIB2_SECTION4_000_MOD::GRIB2_SECTION4_000_T
!>   - @dependency [TYPE] GRIB2_SECTION4_001_MOD::GRIB2_SECTION4_001_T
!>   - @dependency [TYPE] GRIB2_SECTION4_008_MOD::GRIB2_SECTION4_008_T
!>   - @dependency [TYPE] GRIB2_SECTION4_011_MOD::GRIB2_SECTION4_011_T
!>   - @dependency [TYPE] GRIB2_SECTION4_032_MOD::GRIB2_SECTION4_032_T
!>   - @dependency [TYPE] GRIB2_SECTION4_040_MOD::GRIB2_SECTION4_040_T
!>   - @dependency [TYPE] GRIB2_SECTION4_041_MOD::GRIB2_SECTION4_041_T
!>   - @dependency [TYPE] GRIB2_SECTION4_042_MOD::GRIB2_SECTION4_042_T
!>   - @dependency [TYPE] GRIB2_SECTION4_043_MOD::GRIB2_SECTION4_043_T
!>   - @dependency [TYPE] GRIB2_SECTION4_045_MOD::GRIB2_SECTION4_045_T
!>   - @dependency [TYPE] GRIB2_SECTION4_046_MOD::GRIB2_SECTION4_046_T
!>   - @dependency [TYPE] GRIB2_SECTION4_048_MOD::GRIB2_SECTION4_048_T
!>   - @dependency [TYPE] GRIB2_SECTION4_049_MOD::GRIB2_SECTION4_049_T
!>   - @dependency [TYPE] GRIB2_SECTION4_050_MOD::GRIB2_SECTION4_050_T
!>   - @dependency [TYPE] GRIB2_SECTION4_086_MOD::GRIB2_SECTION4_086_T
!>   - @dependency [TYPE] GRIB2_SECTION4_087_MOD::GRIB2_SECTION4_087_T
!>   - @dependency [TYPE] GRIB2_SECTION4_099_MOD::GRIB2_SECTION4_099_T
!>   - @dependency [TYPE] GRIB2_SECTION4_100_MOD::GRIB2_SECTION4_100_T
!>   - @dependency [TYPE] GRIB2_SECTION4_103_MOD::GRIB2_SECTION4_103_T
!>   - @dependency [TYPE] GRIB2_SECTION4_104_MOD::GRIB2_SECTION4_104_T
!>   - @dependency [TYPE] GRIB2_SECTION4_105_MOD::GRIB2_SECTION4_105_T
!>   - @dependency [TYPE] GRIB2_SECTION4_107_MOD::GRIB2_SECTION4_107_T
!>   - @dependency [TYPE] GRIB2_SECTION4_108_MOD::GRIB2_SECTION4_108_T
!>   - @dependency [TYPE] GRIB2_SECTION4_109_MOD::GRIB2_SECTION4_109_T
!>   - @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see GRIB2_SECTION4_000_T
!> @see GRIB2_SECTION4_001_T
!> @see GRIB2_SECTION4_008_T
!> @see GRIB2_SECTION4_011_T
!> @see GRIB2_SECTION4_032_T
!> @see GRIB2_SECTION4_040_T
!> @see GRIB2_SECTION4_041_T
!> @see GRIB2_SECTION4_042_T
!> @see GRIB2_SECTION4_043_T
!> @see GRIB2_SECTION4_045_T
!> @see GRIB2_SECTION4_046_T
!> @see GRIB2_SECTION4_048_T
!> @see GRIB2_SECTION4_049_T
!> @see GRIB2_SECTION4_050_T
!> @see GRIB2_SECTION4_086_T
!> @see GRIB2_SECTION4_087_T
!> @see GRIB2_SECTION4_099_T
!> @see GRIB2_SECTION4_100_T
!> @see GRIB2_SECTION4_103_T
!> @see GRIB2_SECTION4_104_T
!> @see GRIB2_SECTION4_105_T
!> @see GRIB2_SECTION4_107_T
!> @see GRIB2_SECTION4_108_T
!> @see GRIB2_SECTION4_109_T
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MAKE_GRIB2_SECTION4_CFG'
PP_THREAD_SAFE FUNCTION MAKE_GRIB2_SECTION4_CFG( GRIB2_SECTION4, &
&               CFG, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_BASE_A
  USE :: GRIB2_SECTION4_000_MOD,   ONLY: G2S4_000_ALLOCATE
  USE :: GRIB2_SECTION4_001_MOD,   ONLY: G2S4_001_MAKE
  USE :: GRIB2_SECTION4_008_MOD,   ONLY: G2S4_008_ALLOCATE
  USE :: GRIB2_SECTION4_011_MOD,   ONLY: G2S4_011_ALLOCATE
  USE :: GRIB2_SECTION4_032_MOD,   ONLY: G2S4_032_ALLOCATE
  USE :: GRIB2_SECTION4_033_MOD,   ONLY: G2S4_033_ALLOCATE
  USE :: GRIB2_SECTION4_040_MOD,   ONLY: G2S4_040_ALLOCATE
  USE :: GRIB2_SECTION4_041_MOD,   ONLY: G2S4_041_MAKE
  USE :: GRIB2_SECTION4_042_MOD,   ONLY: G2S4_042_ALLOCATE
  USE :: GRIB2_SECTION4_043_MOD,   ONLY: G2S4_043_ALLOCATE
  USE :: GRIB2_SECTION4_045_MOD,   ONLY: G2S4_045_MAKE
  USE :: GRIB2_SECTION4_046_MOD,   ONLY: G2S4_046_ALLOCATE
  USE :: GRIB2_SECTION4_048_MOD,   ONLY: G2S4_048_ALLOCATE
  USE :: GRIB2_SECTION4_049_MOD,   ONLY: G2S4_049_MAKE
#if 0
  USE :: GRIB2_SECTION4_050_MOD,   ONLY: GRIB2_SECTION4_050_T
#endif
  USE :: GRIB2_SECTION4_060_MOD,   ONLY: G2S4_060_MAKE
  USE :: GRIB2_SECTION4_061_MOD,   ONLY: G2S4_061_ALLOCATE
#if 0
  USE :: GRIB2_SECTION4_086_MOD,   ONLY: GRIB2_SECTION4_086_T
  USE :: GRIB2_SECTION4_087_MOD,   ONLY: GRIB2_SECTION4_087_T
#endif
  USE :: GRIB2_SECTION4_099_MOD,   ONLY: G2S4_099_ALLOCATE
  USE :: GRIB2_SECTION4_100_MOD,   ONLY: G2S4_100_ALLOCATE
  USE :: GRIB2_SECTION4_103_MOD,   ONLY: G2S4_103_ALLOCATE
  USE :: GRIB2_SECTION4_104_MOD,   ONLY: G2S4_104_ALLOCATE
#if 0
  USE :: GRIB2_SECTION4_105_MOD,   ONLY: GRIB2_SECTION4_105_T
  USE :: GRIB2_SECTION4_107_MOD,   ONLY: GRIB2_SECTION4_107_T
  USE :: GRIB2_SECTION4_108_MOD,   ONLY: GRIB2_SECTION4_108_T
  USE :: GRIB2_SECTION4_109_MOD,   ONLY: GRIB2_SECTION4_109_T
#endif
  USE :: GRIB2_SECTION4_143_MOD,   ONLY: G2S4_143_MAKE

  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_SECTION_BASE_A), POINTER, INTENT(INOUT) :: GRIB2_SECTION4
  TYPE(YAML_CONFIGURATION_T),          INTENT(IN)    :: CFG
  TYPE(GRIB_ENCODER_OPTIONS_T),        INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ID

   ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_SECTION_4=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATION_ERROR=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INITIALIZATION_ERROR=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SECTION_TYPE=4_JPIB_K


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

  !> Read the encoder type from the configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SECTION_TYPE) READ_GRIB2_SECTION4_TYPE_FROM_CFG( CFG, ID, HOOKS )

  ! Initialize the section
  SELECT CASE( ID )

  CASE (0)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_000_ALLOCATE( GRIB2_SECTION4, HOOKS )

  CASE (1)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_001_MAKE( GRIB2_SECTION4, HOOKS )

  CASE (8)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_008_ALLOCATE( GRIB2_SECTION4, HOOKS )

  CASE (11)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_011_ALLOCATE( GRIB2_SECTION4, HOOKS )

  CASE (32)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_032_ALLOCATE( GRIB2_SECTION4, HOOKS )

  CASE (33)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_033_ALLOCATE( GRIB2_SECTION4, HOOKS )

  CASE (40)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_040_ALLOCATE( GRIB2_SECTION4, HOOKS )

  CASE (41)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_041_MAKE( GRIB2_SECTION4, HOOKS )

  CASE (42)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_042_ALLOCATE( GRIB2_SECTION4, HOOKS )

  CASE (43)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_043_ALLOCATE( GRIB2_SECTION4, HOOKS )
    
  CASE (45)
  
    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_045_MAKE( GRIB2_SECTION4, HOOKS )

  CASE (46)
  
    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_046_ALLOCATE( GRIB2_SECTION4, HOOKS )

  CASE (48)
  
    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_048_ALLOCATE( GRIB2_SECTION4, HOOKS )

  CASE (49)
  
    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_049_MAKE( GRIB2_SECTION4, HOOKS )

#if 0
  CASE (50)

    ALLOCATE( GRIB2_SECTION4_050_T::GRIB2_SECTION4, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )
#endif
    
  CASE (60)
  
    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_060_MAKE( GRIB2_SECTION4, HOOKS )
    
  CASE (61)
  
    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_061_ALLOCATE( GRIB2_SECTION4, HOOKS )

#if 0
  CASE (86)

    ALLOCATE( GRIB2_SECTION4_086_T::GRIB2_SECTION4, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  CASE (87)

    ALLOCATE( GRIB2_SECTION4_087_T::GRIB2_SECTION4, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )
#endif

  CASE (99)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_099_ALLOCATE( GRIB2_SECTION4, HOOKS )

  CASE (100)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_100_ALLOCATE( GRIB2_SECTION4, HOOKS )

  CASE (103)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_103_ALLOCATE( GRIB2_SECTION4, HOOKS )

  CASE (104)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_104_ALLOCATE( GRIB2_SECTION4, HOOKS )

#if 0
  CASE (105)

    ALLOCATE( GRIB2_SECTION4_105_T::GRIB2_SECTION4, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  CASE (107)

    ALLOCATE( GRIB2_SECTION4_107_T::GRIB2_SECTION4, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  CASE (108)

    ALLOCATE( GRIB2_SECTION4_108_T::GRIB2_SECTION4, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  CASE (109)

    ALLOCATE( GRIB2_SECTION4_109_T::GRIB2_SECTION4, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )
#endif
  CASE (143)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_143_MAKE( GRIB2_SECTION4, HOOKS )

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_SECTION_4 )

  END SELECT

  !> Initialization of the section
  PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  GRIB2_SECTION4%INIT( CFG, OPT, HOOKS )

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
    CHARACTER(LEN=32) :: TMP

    ! Initialize error frame
    PP_DEBUG_PUSH_FRAME()

    TMP = REPEAT(' ', 32)
    WRITE(TMP,'(I32)')  ID

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNKNOWN_SECTION_4)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown section4 number: '//TRIM(ADJUSTL(TMP)) )
    CASE (ERRFLAG_ALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating section4 number: '//TRIM(ADJUSTL(TMP)) )
    CASE (ERRFLAG_INITIALIZATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error initializing section4 number: '//TRIM(ADJUSTL(TMP)) )
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

END FUNCTION MAKE_GRIB2_SECTION4_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



!>
!> @brief Factory function for creating or initializing GRIB2 Section 4 objects.
!>
!> This function acts as a factory for creating or initializing a GRIB2 Section 4 object
!> based on the provided parameters. It assigns the proper type (`GRIB2_SECTION4_000_T`)
!> to the `GRIB2_SECTION4` object and configures it using the provided model parameters,
!> ID, and YAML configuration. If verbose mode is enabled, additional debug information
!> is output during the process.
!>
!> @param [inout] GRIB2_SECTION4 The GRIB2 Section 4 object that will be created or initialized.
!>                              It must be a pointer of type `GRIB_SECTION_BASE_A`.
!> @param [in]    MSG   All the mars keywords needed to describe the field `FORTRAN_MESSAGE_T`.
!> @param [in]    PAR   All information outside mars keywords needed to describe the field `PARAMETRIZATION_T`.
!> @param [in]    OPT   The encoder options structure of type `ENCODER_OPTIONS_T`.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Section that can be constructed with this factory
!>   - `GRIB2_SECTION4_000_T`
!>   - `GRIB2_SECTION4_001_T`
!>   - `GRIB2_SECTION4_008_T`
!>   - `GRIB2_SECTION4_011_T`
!>   - `GRIB2_SECTION4_032_T`
!>   - `GRIB2_SECTION4_040_T`
!>   - `GRIB2_SECTION4_041_T`
!>   - `GRIB2_SECTION4_042_T`
!>   - `GRIB2_SECTION4_043_T`
!>   - `GRIB2_SECTION4_044_T`
!>   - `GRIB2_SECTION4_045_T`
!>   - `GRIB2_SECTION4_046_T`
!>   - `GRIB2_SECTION4_047_T`
!>   - `GRIB2_SECTION4_048_T`
!>   - `GRIB2_SECTION4_049_T`
!>   - `GRIB2_SECTION4_050_T`
!>   - `GRIB2_SECTION4_086_T`
!>   - `GRIB2_SECTION4_087_T`
!>   - `GRIB2_SECTION4_099_T`
!>   - `GRIB2_SECTION4_100_T`
!>   - `GRIB2_SECTION4_103_T`
!>   - `GRIB2_SECTION4_104_T`
!>   - `GRIB2_SECTION4_105_T`
!>   - `GRIB2_SECTION4_107_T`
!>   - `GRIB2_SECTION4_108_T`
!>   - `GRIB2_SECTION4_109_T`
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] OM_CORE_MOD::JPIB_K
!>   - @dependency [TYPE] GRIB2_SECTION4_000_MOD::GRIB2_SECTION4_000_T
!>   - @dependency [TYPE] GRIB2_SECTION4_001_MOD::GRIB2_SECTION4_001_T
!>   - @dependency [TYPE] GRIB2_SECTION4_008_MOD::GRIB2_SECTION4_008_T
!>   - @dependency [TYPE] GRIB2_SECTION4_011_MOD::GRIB2_SECTION4_011_T
!>   - @dependency [TYPE] GRIB2_SECTION4_032_MOD::GRIB2_SECTION4_032_T
!>   - @dependency [TYPE] GRIB2_SECTION4_040_MOD::GRIB2_SECTION4_040_T
!>   - @dependency [TYPE] GRIB2_SECTION4_041_MOD::GRIB2_SECTION4_041_T
!>   - @dependency [TYPE] GRIB2_SECTION4_042_MOD::GRIB2_SECTION4_042_T
!>   - @dependency [TYPE] GRIB2_SECTION4_043_MOD::GRIB2_SECTION4_043_T
!>   - @dependency [TYPE] GRIB2_SECTION4_044_MOD::GRIB2_SECTION4_044_T
!>   - @dependency [TYPE] GRIB2_SECTION4_045_MOD::GRIB2_SECTION4_045_T
!>   - @dependency [TYPE] GRIB2_SECTION4_046_MOD::GRIB2_SECTION4_046_T
!>   - @dependency [TYPE] GRIB2_SECTION4_047_MOD::GRIB2_SECTION4_047_T
!>   - @dependency [TYPE] GRIB2_SECTION4_048_MOD::GRIB2_SECTION4_048_T
!>   - @dependency [TYPE] GRIB2_SECTION4_049_MOD::GRIB2_SECTION4_049_T
!>   - @dependency [TYPE] GRIB2_SECTION4_050_MOD::GRIB2_SECTION4_050_T
!>   - @dependency [TYPE] GRIB2_SECTION4_086_MOD::GRIB2_SECTION4_086_T
!>   - @dependency [TYPE] GRIB2_SECTION4_087_MOD::GRIB2_SECTION4_087_T
!>   - @dependency [TYPE] GRIB2_SECTION4_099_MOD::GRIB2_SECTION4_099_T
!>   - @dependency [TYPE] GRIB2_SECTION4_100_MOD::GRIB2_SECTION4_100_T
!>   - @dependency [TYPE] GRIB2_SECTION4_103_MOD::GRIB2_SECTION4_103_T
!>   - @dependency [TYPE] GRIB2_SECTION4_104_MOD::GRIB2_SECTION4_104_T
!>   - @dependency [TYPE] GRIB2_SECTION4_105_MOD::GRIB2_SECTION4_105_T
!>   - @dependency [TYPE] GRIB2_SECTION4_107_MOD::GRIB2_SECTION4_107_T
!>   - @dependency [TYPE] GRIB2_SECTION4_108_MOD::GRIB2_SECTION4_108_T
!>   - @dependency [TYPE] GRIB2_SECTION4_109_MOD::GRIB2_SECTION4_109_T
!>   - @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see GRIB2_SECTION4_000_T
!> @see GRIB2_SECTION4_001_T
!> @see GRIB2_SECTION4_008_T
!> @see GRIB2_SECTION4_011_T
!> @see GRIB2_SECTION4_032_T
!> @see GRIB2_SECTION4_040_T
!> @see GRIB2_SECTION4_041_T
!> @see GRIB2_SECTION4_042_T
!> @see GRIB2_SECTION4_043_T
!> @see GRIB2_SECTION4_044_T
!> @see GRIB2_SECTION4_045_T
!> @see GRIB2_SECTION4_046_T
!> @see GRIB2_SECTION4_047_T
!> @see GRIB2_SECTION4_048_T
!> @see GRIB2_SECTION4_049_T
!> @see GRIB2_SECTION4_050_T
!> @see GRIB2_SECTION4_086_T
!> @see GRIB2_SECTION4_087_T
!> @see GRIB2_SECTION4_099_T
!> @see GRIB2_SECTION4_100_T
!> @see GRIB2_SECTION4_103_T
!> @see GRIB2_SECTION4_104_T
!> @see GRIB2_SECTION4_105_T
!> @see GRIB2_SECTION4_107_T
!> @see GRIB2_SECTION4_108_T
!> @see GRIB2_SECTION4_109_T
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MAKE_GRIB2_SECTION4_LAZY'
PP_THREAD_SAFE FUNCTION MAKE_GRIB2_SECTION4_LAZY( GRIB2_SECTION4, &
&               MSG, PAR, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_BASE_A
  USE :: GRIB2_SECTION4_000_MOD,   ONLY: G2S4_000_ALLOCATE
  USE :: GRIB2_SECTION4_001_MOD,   ONLY: G2S4_001_MAKE
  USE :: GRIB2_SECTION4_008_MOD,   ONLY: G2S4_008_ALLOCATE
  USE :: GRIB2_SECTION4_011_MOD,   ONLY: G2S4_011_ALLOCATE
  USE :: GRIB2_SECTION4_032_MOD,   ONLY: G2S4_032_ALLOCATE
  USE :: GRIB2_SECTION4_040_MOD,   ONLY: G2S4_040_ALLOCATE
  USE :: GRIB2_SECTION4_041_MOD,   ONLY: G2S4_041_MAKE
  USE :: GRIB2_SECTION4_042_MOD,   ONLY: G2S4_042_ALLOCATE
  USE :: GRIB2_SECTION4_043_MOD,   ONLY: G2S4_043_ALLOCATE
#if 0
  USE :: GRIB2_SECTION4_044_MOD,   ONLY: GRIB2_SECTION4_044_T
  USE :: GRIB2_SECTION4_045_MOD,   ONLY: GRIB2_SECTION4_045_T
  USE :: GRIB2_SECTION4_046_MOD,   ONLY: GRIB2_SECTION4_046_T
  USE :: GRIB2_SECTION4_047_MOD,   ONLY: GRIB2_SECTION4_047_T
  USE :: GRIB2_SECTION4_048_MOD,   ONLY: GRIB2_SECTION4_048_T
  USE :: GRIB2_SECTION4_049_MOD,   ONLY: GRIB2_SECTION4_049_T
  USE :: GRIB2_SECTION4_050_MOD,   ONLY: GRIB2_SECTION4_050_T
  USE :: GRIB2_SECTION4_086_MOD,   ONLY: GRIB2_SECTION4_086_T
  USE :: GRIB2_SECTION4_087_MOD,   ONLY: GRIB2_SECTION4_087_T
#endif
  USE :: GRIB2_SECTION4_099_MOD,   ONLY: G2S4_099_ALLOCATE
  USE :: GRIB2_SECTION4_100_MOD,   ONLY: G2S4_100_ALLOCATE
  USE :: GRIB2_SECTION4_103_MOD,   ONLY: G2S4_103_ALLOCATE
  USE :: GRIB2_SECTION4_104_MOD,   ONLY: G2S4_104_ALLOCATE
#if 0
  USE :: GRIB2_SECTION4_105_MOD,   ONLY: GRIB2_SECTION4_105_T
  USE :: GRIB2_SECTION4_107_MOD,   ONLY: GRIB2_SECTION4_107_T
  USE :: GRIB2_SECTION4_108_MOD,   ONLY: GRIB2_SECTION4_108_T
  USE :: GRIB2_SECTION4_109_MOD,   ONLY: GRIB2_SECTION4_109_T
#endif
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

#if defined( PP_HAS_GET_GRIB2_SECTION4_ID_FROM_MESSAGE )
  !> Symbols imported from the mapping module
  USE :: MARS2GRIB_MAPPING_MOD, ONLY: GET_GRIB2_SECTION4_ID_FROM_MESSAGE
#endif

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_SECTION_BASE_A), POINTER, INTENT(INOUT) :: GRIB2_SECTION4
  TYPE(FORTRAN_MESSAGE_T),             INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),             INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T),        INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ID
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

   ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_SECTION4_ID=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_SECTION_4=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATION_ERROR=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INITIALIZATION_ERROR=3_JPIB_K

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


#if defined( PP_HAS_GET_GRIB2_SECTION4_ID_FROM_MESSAGE )
  !> Get the section0 ID from the message
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_SECTION4_ID) GET_GRIB2_SECTION4_ID_FROM_MESSAGE( MSG, PAR, OPT, ID, HOOKS )

  ! Initialize the section
  SELECT CASE( ID )

  CASE (0)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_000_ALLOCATE( GRIB2_SECTION4, HOOKS )

  CASE (1)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_001_MAKE( GRIB2_SECTION4, HOOKS )

  CASE (8)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_008_ALLOCATE( GRIB2_SECTION4, HOOKS )

  CASE (11)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_011_ALLOCATE( GRIB2_SECTION4, HOOKS )

  CASE (32)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_032_ALLOCATE( GRIB2_SECTION4, HOOKS )

  CASE (40)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_040_ALLOCATE( GRIB2_SECTION4, HOOKS )

  CASE (41)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_041_MAKE( GRIB2_SECTION4, HOOKS )

  CASE (42)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_042_ALLOCATE( GRIB2_SECTION4, HOOKS )

  CASE (43)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_043_ALLOCATE( GRIB2_SECTION4, HOOKS )
#if 0
  CASE (44)

    ALLOCATE( GRIB2_SECTION4_044_T::GRIB2_SECTION4, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  CASE (45)

    ALLOCATE( GRIB2_SECTION4_045_T::GRIB2_SECTION4, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  CASE (46)

    ALLOCATE( GRIB2_SECTION4_046_T::GRIB2_SECTION4, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  CASE (47)

    ALLOCATE( GRIB2_SECTION4_047_T::GRIB2_SECTION4, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  CASE (48)

    ALLOCATE( GRIB2_SECTION4_048_T::GRIB2_SECTION4, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  CASE (49)

    ALLOCATE( GRIB2_SECTION4_049_T::GRIB2_SECTION4, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  CASE (50)

    ALLOCATE( GRIB2_SECTION4_050_T::GRIB2_SECTION4, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  CASE (86)

    ALLOCATE( GRIB2_SECTION4_086_T::GRIB2_SECTION4, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  CASE (87)

    ALLOCATE( GRIB2_SECTION4_087_T::GRIB2_SECTION4, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )
#endif

  CASE (99)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_099_ALLOCATE( GRIB2_SECTION4, HOOKS )

  CASE (100)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_100_ALLOCATE( GRIB2_SECTION4, HOOKS )

  CASE (103)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_103_ALLOCATE( GRIB2_SECTION4, HOOKS )

  CASE (104)

    PP_TRYCALL(ERRFLAG_ALLOCATION_ERROR) G2S4_104_ALLOCATE( GRIB2_SECTION4, HOOKS )

#if 0
  CASE (105)

    ALLOCATE( GRIB2_SECTION4_105_T::GRIB2_SECTION4, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  CASE (107)

    ALLOCATE( GRIB2_SECTION4_107_T::GRIB2_SECTION4, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  CASE (108)

    ALLOCATE( GRIB2_SECTION4_108_T::GRIB2_SECTION4, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  CASE (109)

    ALLOCATE( GRIB2_SECTION4_109_T::GRIB2_SECTION4, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )
#endif
  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_SECTION_4 )

  END SELECT

  !> Initialization of the section
  PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  GRIB2_SECTION4%INIT( MSG, PAR, OPT, HOOKS )
#else
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNABLE_TO_GET_SECTION4_ID )
#endif

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
    CHARACTER(LEN=32) :: TMP

    ! Initialize error frame
    PP_DEBUG_PUSH_FRAME()

    TMP = REPEAT(' ', 32)
    WRITE(TMP,'(I32)')  ID

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_GET_SECTION4_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get section4 ID' )
    CASE (ERRFLAG_UNKNOWN_SECTION_4)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown section4 number: '//TRIM(ADJUSTL(TMP)) )
    CASE (ERRFLAG_ALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating section4 number: '//TRIM(ADJUSTL(TMP)) )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_INITIALIZATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error initializing section4 number: '//TRIM(ADJUSTL(TMP)) )
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

END FUNCTION MAKE_GRIB2_SECTION4_LAZY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Destroys a GRIB2 Section 4 structure.
!>
!> This function takes a GRIB2 Section 4 object (`GRIB2_SECTION4`) and performs
!> the necessary cleanup, freeing any associated resources. It also supports a
!> `VERBOSE` mode for detailed output during the destruction process.
!>
!> @section interface
!> @param[in,out] GRIB2_SECTION4 The GRIB2 Section 4 object to be destroyed.
!>                              The structure is modified in place.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [PARAMETER] OM_CORE_MOD::JPIB_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see MAKE_GRIB2_SECTION4
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DESTROY_GRIB2_SECTION4'
PP_THREAD_SAFE FUNCTION DESTROY_GRIB2_SECTION4( GRIB2_SECTION4, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_BASE_A
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_SECTION_BASE_A), POINTER, INTENT(INOUT) :: GRIB2_SECTION4
  TYPE(GRIB_ENCODER_OPTIONS_T),        INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATION_ERROR=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FREE_ERROR=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ASSOCIATED=1_JPIB_K

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

  ! Check if the section is associated
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(GRIB2_SECTION4), ERRFLAG_NOT_ASSOCIATED )

  !> Free the section
  PP_TRYCALL(ERRFLAG_FREE_ERROR)  GRIB2_SECTION4%FREE( OPT, HOOKS )

  ! Initialize the section
  DEALLOCATE( GRIB2_SECTION4, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
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
    CASE (ERRFLAG_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'section0 to destroy is not associated' )
    CASE (ERRFLAG_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating section0 number' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_FREE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error destructing section0' )
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

END FUNCTION DESTROY_GRIB2_SECTION4
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Read from the YAML configuration the type of the GRIB2 Section 4 object.
!>
!> @section interface
!> @param [in]     CFG             YAML configuration object used to configure the GRIB2 Section 4 object.
!> @param [out]    SECTION4_TYPE   Identifier of the section0 type read from the configuration.
!> @param [in,out] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [PARAMETER] OM_CORE_MOD::JPIB_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see MAKE_GRIB2_SECTION4
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_GRIB2_SECTION4_TYPE_FROM_CFG'
PP_THREAD_SAFE FUNCTION READ_GRIB2_SECTION4_TYPE_FROM_CFG( CFG, SECTION4_TYPE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_STRING
  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_IS_INTEGER
  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_TO_INTEGER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CFG
  INTEGER(KIND=JPIB_K),       INTENT(OUT)   :: SECTION4_TYPE
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  CHARACTER(LEN=:), ALLOCATABLE :: CSECTION4_TYPE
  LOGICAL :: HAS_SECTION4
  LOGICAL :: SECTION4_IS_INTEGER

   ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED_AFTER_READ=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATION_ERROR=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_NOT_PRESENT=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_IS_NOT_INTEGER=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_KEY=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_READ_ERROR=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_STRING_IS_INTEGER=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_STRING_TO_INTEGER=8_JPIB_K


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

  ! Initialization
  SECTION4_TYPE = -1_JPIB_K

  !> Check if configuration has the SECTION4 key
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_KEY) YAML_CONFIGURATION_HAS_KEY( CFG, SECTION4_KEY, HAS_SECTION4, HOOKS )

  !> Read the SECTION4 template number
  IF ( HAS_SECTION4 ) THEN
    PP_TRYCALL(ERRFLAG_READ_ERROR) YAML_READ_STRING( CFG, SECTION4_KEY, CSECTION4_TYPE, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CSECTION4_TYPE), ERRFLAG_NOT_ALLOCATED_AFTER_READ )
    PP_TRYCALL( ERRFLAG_STRING_IS_INTEGER ) STRING_IS_INTEGER( CSECTION4_TYPE, SECTION4_IS_INTEGER, HOOKS )
    IF ( SECTION4_IS_INTEGER ) THEN
       PP_TRYCALL( ERRFLAG_STRING_TO_INTEGER ) STRING_TO_INTEGER( CSECTION4_TYPE, SECTION4_TYPE, HOOKS )
    ELSE
      ! TODO: Somehow decide automatically whcih SECTION4 to use. String can be
      !       a label for the method to use.
      PP_DEBUG_CRITICAL_THROW( ERRFLAG_KEY_IS_NOT_INTEGER )
    ENDIF
    DEALLOCATE(CSECTION4_TYPE, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )
  ELSE
    ! TODO
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_KEY_NOT_PRESENT )
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
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
    CASE (ERRFLAG_NOT_ALLOCATED_AFTER_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'SECTION4 ctype not allocated after reading' )
    CASE (ERRFLAG_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating SECTION4 number' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_KEY_NOT_PRESENT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key is not present' )
    CASE (ERRFLAG_KEY_IS_NOT_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key is not integer' )

    CASE (ERRFLAG_UNABLE_TO_READ_KEY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error calling check key' )
    CASE (ERRFLAG_READ_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error reading key' )
    CASE (ERRFLAG_STRING_IS_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in checking if string is an integer' )
    CASE (ERRFLAG_STRING_TO_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in converting string to integer' )
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

END FUNCTION READ_GRIB2_SECTION4_TYPE_FROM_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE GRIB2_SECTION4_FACTORY_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
