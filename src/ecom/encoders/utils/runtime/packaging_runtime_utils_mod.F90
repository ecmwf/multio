! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'packaging_runtime_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'PACKAGING_RUNTIME_UTILS_MOD'
MODULE PACKAGING_RUNTIME_UTILS_MOD

IMPLICIT NONE

! Default visibility
PRIVATE

! Configuration variables
LOGICAL :: LFORCEZ
LOGICAL :: LFORCELNSP

! Whitelist of public symbols
PUBLIC :: PACKAGING_RUNTIME_INIT
PUBLIC :: PACKAGING_RUNTIME_FREE
PUBLIC :: SET_MISSING_VALUE_GG_GRIBX_ATM
PUBLIC :: SET_MISSING_VALUE_GG_GRIBX_WAM
PUBLIC :: SET_BITS_PER_VALUE_GG_GRIBX_ATM
PUBLIC :: SET_PACKING_SH_GRIBX_ATM
PUBLIC :: NEED_FIT_SPECTRUM

CONTAINS


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PACKAGING_RUNTIME_INIT'
SUBROUTINE PACKAGING_RUNTIME_INIT( CFG, MODEL_PARAMS )

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

  LFORCEZ    = .FALSE.
  LFORCELNSP = .FALSE.

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE PACKAGING_RUNTIME_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PACKAGING_RUNTIME_FREE'
SUBROUTINE PACKAGING_RUNTIME_FREE( )

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

END SUBROUTINE PACKAGING_RUNTIME_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_MISSING_VALUE_GG_GRIBX_ATM'
SUBROUTINE SET_MISSING_VALUE_GG_GRIBX_ATM( MODEL_PARAMS, GRIB_INFO, MSG, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! If missing values are present in the field then configure it
  IF ( MSG%NUNDF_ .GT. 0 ) THEN
    PP_METADATA_SET( METADATA,  'bitmapPresent', .TRUE. )
    PP_METADATA_SET( METADATA,  'missingValue', MSG%XUNDF_ )
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SET_MISSING_VALUE_GG_GRIBX_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_MISSING_VALUE_GG_GRIBX_WAM'
SUBROUTINE SET_MISSING_VALUE_GG_GRIBX_WAM( MODEL_PARAMS, GRIB_INFO, MSG, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,        ONLY: OM_WAM_MSG_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(OM_WAM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables
  INTEGER(KIND=JPIB_K) :: IBITS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! If missing values are present in the field then configure it
  IF ( MSG%NUNDF_ .GT. 0 ) THEN
    PP_METADATA_SET( METADATA,  'bitmapPresent', .TRUE. )
    PP_METADATA_SET( METADATA,  'missingValue', MSG%XUNDF_ )
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SET_MISSING_VALUE_GG_GRIBX_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_BITS_PER_VALUE_GG_GRIBX_ATM'
SUBROUTINE SET_BITS_PER_VALUE_GG_GRIBX_ATM( MODEL_PARAMS, GRIB_INFO, MSG, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables
  INTEGER(KIND=JPIB_K) :: IBITS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  PP_DEBUG_CRITICAL_COND_THROW( MSG%IPREF_.LE.0, 1 )
  PP_DEBUG_CRITICAL_COND_THROW( MSG%IPREF_.GT.7, 2 )

  IBITS = GRIB_INFO%BITS_PER_VALUE_( MSG%IPREF_ )

  PP_DEBUG_CRITICAL_COND_THROW( IBITS.LT.1,  3 )
  PP_DEBUG_CRITICAL_COND_THROW( IBITS.GT.64, 4 )

  PP_METADATA_SET( METADATA,  'numberOfBitsContainingEachPackedValue', IBITS )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'IPrefix lower than 1' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'IPrefix greater than 7' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'BitsPerValue lower than 1' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'BitsPerValue greater than 64' )
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

END SUBROUTINE SET_BITS_PER_VALUE_GG_GRIBX_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_PACKING_SH_GRIBX_ATM'
SUBROUTINE SET_PACKING_SH_GRIBX_ATM( MODEL_PARAMS, GRIB_INFO, MSG, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: JPRB_K
  USE :: OM_CORE_MOD,        ONLY: JPRD_K
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: OM_CORE_MOD,        ONLY: MODEL_LEVEL_E
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: GRIB_CODES_MOD,     ONLY: NGRBLNSP
  USE :: GRIB_CODES_MOD,     ONLY: NGRBZ

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables
  INTEGER(KIND=JPIB_K) :: IBITS
  INTEGER(KIND=JPIB_K) :: ISPEC2
  INTEGER(KIND=JPIB_K) :: IBITSSH
  INTEGER(KIND=JPIB_K) :: ISTRUNC
  REAL(KIND=JPRB_K)    :: ZP
  REAL(KIND=JPRB_K)    :: ZBETA0
  REAL(KIND=JPRB_K)    :: ZBETA1
  REAL(KIND=JPRD_K)    :: ZMIN
  REAL(KIND=JPRD_K)    :: ZMAX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ISPEC2 = (MODEL_PARAMS%GEO_%ISMAX+1)*(MODEL_PARAMS%GEO_%ISMAX+2)
  IF ( MSG%PARAM_ID_ .EQ. NGRBLNSP .AND. &
&      MSG%IPREF_ .EQ. MODEL_LEVEL_E ) THEN
    IBITSSH = MODEL_PARAMS%SIM_%NBITSSHLNSP
  ELSE
    IBITSSH = 16
  ENDIF

  IF( MODEL_PARAMS%GEO_%ISMAX .GE. 213 ) THEN
    ISTRUNC = 20
  ELSE
    ISTRUNC = MIN( 10 ,MODEL_PARAMS%GEO_%ISMAX )
  ENDIF
  PP_METADATA_SET( METADATA,  'numberOfBitsContainingEachPackedValue', IBITSSH )


  PP_LOG_DEVELOP_STR( 'GRIB2: Spherical harmonics Packaging 0' )
  IF ( LFORCEZ  .AND. &
&      MSG%PARAM_ID_  .EQ. NGRBZ  .AND. &
&      MSG%IPREF_     .EQ. MODEL_LEVEL_E ) THEN
    PP_METADATA_SET( METADATA,  'laplacianOperator',0.5_JPRB_K)
  ELSEIF ( LFORCELNSP    .AND. &
&          MSG%PARAM_ID_ .EQ. NGRBLNSP  .AND. &
&          MSG%IPREF_    .EQ. MODEL_LEVEL_E ) THEN
    PP_METADATA_SET( METADATA,  'laplacianOperator',0.5_JPRB_K)
  ELSEIF ( NEED_FIT_SPECTRUM(MODEL_PARAMS%GEO_%ISMAX) ) THEN
    PP_METADATA_SET( METADATA,  'laplacianOperator',MSG%ZP_)
  ELSE
    PP_METADATA_SET( METADATA,  'laplacianOperator', 0.0_JPRB_K )
  ENDIF
  PP_LOG_DEVELOP_STR( 'GRIB2: Spherical harmonics Packaging 1' )
  PP_METADATA_SET( METADATA,  'subSetJ', ISTRUNC )
  PP_METADATA_SET( METADATA,  'subSetK', ISTRUNC )
  PP_METADATA_SET( METADATA,  'subSetM', ISTRUNC )
  PP_LOG_DEVELOP_STR( 'GRIB2: Spherical harmonics Packaging 2' )

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

 ! Exit point on success
  RETURN


END SUBROUTINE SET_PACKING_SH_GRIBX_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> The output number of harmonics (OS) is a function of the input number of harmonics (NSMAX) as follows:
!>
!> - When 0  < NSMAX <= 10,  OS = NSMAX
!> - When 10 < NSMAX <= 212, OS = 10
!> - When      NSMAX >  212, OS = 20
!>
!> Thus, interpolation of the spectrum is necessary whenever NSMAX exceeds 10.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'NEED_FIT_SPECTRUM'
FUNCTION NEED_FIT_SPECTRUM( NSMAX ) RESULT(NFT)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: NSMAX

  ! Function result
  LOGICAL :: NFT

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  NFT = NSMAX > 10

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

 ! Exit point on success
  RETURN

END FUNCTION NEED_FIT_SPECTRUM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE PACKAGING_RUNTIME_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
