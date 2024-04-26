#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'packaging_assumptions_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'PACKAGING_ASSUMPTIONS_MOD'
MODULE PACKAGING_ASSUMPTIONS_MOD
IMPLICIT NONE

! Default visibility
PRIVATE

! Flag used to enable compression
LOGICAL :: LG_ENABLE_COMPRESSION_GRIDDED = .FALSE.

! Whitelist of public symbols
PUBLIC :: COMPUTE_BITS_PER_VALUE_DEFAULT
PUBLIC :: COMPUTE_BITS_PER_VALUE
PUBLIC :: COMPUTE_PACKING_TYPE
PUBLIC :: PACKAGING_ASSUMPTIONS_INIT
PUBLIC :: PACKAGING_ASSUMPTIONS_FREE

CONTAINS

#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PACKAGING_ASSUMPTIONS_INIT'
SUBROUTINE PACKAGING_ASSUMPTIONS_INIT( CFG, MODEL_PARAMS )

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

  ! Local variables
  LOGICAL :: EX
  LOGICAL :: LTMP
  TYPE(FCKIT_CONFIGURATION) :: PACK

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( CFG%GET( 'packaging', PACK ) ) THEN
    EX = PACK%GET( 'enable-compression-gridded', LTMP  )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, 1 )
    LG_ENABLE_COMPRESSION_GRIDDED = LTMP
    CALL PACK%FINAL()
  ELSE
    LG_ENABLE_COMPRESSION_GRIDDED = .FALSE.
    CALL PACK%FINAL()
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read the unable compression flag' )
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

END SUBROUTINE PACKAGING_ASSUMPTIONS_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PACKAGING_ASSUMPTIONS_FREE'
SUBROUTINE PACKAGING_ASSUMPTIONS_FREE( )

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

END SUBROUTINE PACKAGING_ASSUMPTIONS_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMPUTE_BITS_PER_VALUE_DEFAULT'
FUNCTION COMPUTE_BITS_PER_VALUE_DEFAULT( MODEL_PARAMS, KGRIBID, KPREFIX ) RESULT(IBITS)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),    INTENT(IN) :: MODEL_PARAMS
  INTEGER(KIND=JPIB_K), INTENT(IN) :: KGRIBID
  INTEGER(KIND=JPIB_K), INTENT(IN) :: KPREFIX

  ! Function Result
  INTEGER(KIND=JPIB_K) :: IBITS

  ! Local variables
  LOGICAL, DIMENSION(3) :: CONDITIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Conditions to use NBITSEXPR
  CONDITIONS(1) = MODEL_PARAMS%SIM_%NBITSEXPR>0
  CONDITIONS(2) = (KGRIBID .GE. 80)
  CONDITIONS(3) = (KGRIBID .LE. 120)

  ! NBITSEXPR   - Number of bits for GRIB encoding of experimental parameters (default=-1 in which case multio is deciding)
  IF ( ALL(CONDITIONS) ) THEN
    IBITS = MODEL_PARAMS%SIM_%NBITSEXPR
  ELSE
    IBITS = LOOKUP_BITS_PER_VALUE_DEFAULT( MODEL_PARAMS, KGRIBID, KPREFIX, LG_ENABLE_COMPRESSION_GRIDDED )
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION COMPUTE_BITS_PER_VALUE_DEFAULT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMPUTE_PACKING_TYPE'
FUNCTION COMPUTE_PACKING_TYPE( MODEL_PARAMS, KGRIBID, KREPRES ) RESULT(PACKING_TYPE)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T

  USE :: OM_CORE_MOD, ONLY: PACKING_TYPE_GRIB_SIMPLE_E
  USE :: OM_CORE_MOD, ONLY: PACKING_TYPE_GRIB_COMPLEX_E
  USE :: OM_CORE_MOD, ONLY: REPRES_GRIDDED_E
  USE :: OM_CORE_MOD, ONLY: REPRES_SPECTRAL_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),    INTENT(IN) :: MODEL_PARAMS
  INTEGER(KIND=JPIB_K), INTENT(IN) :: KGRIBID
  INTEGER(KIND=JPIB_K), INTENT(IN) :: KREPRES

  ! Function Result
  INTEGER(KIND=JPIB_K) :: PACKING_TYPE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  SELECT CASE ( KREPRES )

  CASE (REPRES_GRIDDED_E)
    PACKING_TYPE = PACKING_TYPE_GRIB_SIMPLE_E
  CASE (REPRES_SPECTRAL_E)
    PACKING_TYPE = PACKING_TYPE_GRIB_COMPLEX_E
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW(1)
  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown representation' )
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

END FUNCTION COMPUTE_PACKING_TYPE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMPUTE_BITS_PER_VALUE'
FUNCTION COMPUTE_BITS_PER_VALUE( MODEL_PARAMS, KGRIBID, KPREFIX, KREPRES, ENABLE_COMPRESSION ) RESULT(IBITS)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD, ONLY: REPRES_GRIDDED_E
  USE :: OM_CORE_MOD, ONLY: REPRES_SPECTRAL_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),    INTENT(IN) :: MODEL_PARAMS
  INTEGER(KIND=JPIB_K), INTENT(IN) :: KGRIBID
  INTEGER(KIND=JPIB_K), INTENT(IN) :: KPREFIX
  INTEGER(KIND=JPIB_K), INTENT(IN) :: KREPRES
  LOGICAL,              INTENT(IN) :: ENABLE_COMPRESSION

  ! Function Result
  INTEGER(KIND=JPIB_K) :: IBITS

  ! Local variables
  LOGICAL, DIMENSION(3) :: CONDITIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Conditions to use NBITSEXPR
  CONDITIONS(1) = MODEL_PARAMS%SIM_%NBITSEXPR>0
  CONDITIONS(2) = (KGRIBID .GE. 80)
  CONDITIONS(3) = (KGRIBID .LE. 120)

  ! NBITSEXPR   - Number of bits for GRIB encoding of experimental parameters (default=-1 in which case multio is deciding)
  SELECT CASE (KREPRES)

  CASE (REPRES_GRIDDED_E)

    IF ( ALL(CONDITIONS) ) THEN
      IBITS = MODEL_PARAMS%SIM_%NBITSEXPR
    ELSE
      IBITS = LOOKUP_BITS_PER_VALUE_DEFAULT( MODEL_PARAMS, KGRIBID, KPREFIX, ENABLE_COMPRESSION )
    ENDIF

  CASE (REPRES_SPECTRAL_E)

    IBITS = 16_JPIB_K

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( 1 )

  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown representation' )
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

END FUNCTION COMPUTE_BITS_PER_VALUE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'LOOKUP_BITS_PER_VALUE_DEFAULT'
FUNCTION LOOKUP_BITS_PER_VALUE_DEFAULT(  MODEL_PARAMS, KGRIBID, KPREFIX, ENABLE_COMPRESSION ) RESULT(IBITS_PER_VALUE)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T

  USE :: OM_CORE_MOD, ONLY: MODEL_LEVEL_E
  USE :: OM_CORE_MOD, ONLY: PRESSURE_LEVEL_E

  USE :: GRIB_CODES_MOD, ONLY: NGRBCC
  USE :: GRIB_CODES_MOD, ONLY: NGRBSD
  USE :: GRIB_CODES_MOD, ONLY: NGRBFSR
  USE :: GRIB_CODES_MOD, ONLY: NGRBCLWC
  USE :: GRIB_CODES_MOD, ONLY: NGRBCIWC
  USE :: GRIB_CODES_MOD, ONLY: NGRBCLBT
  USE :: GRIB_CODES_MOD, ONLY: NGRBCSBT

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS


IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),    INTENT(IN) :: MODEL_PARAMS
  INTEGER(KIND=JPIB_K), INTENT(IN) :: KGRIBID
  INTEGER(KIND=JPIB_K), INTENT(IN) :: KPREFIX
  LOGICAL,              INTENT(IN) :: ENABLE_COMPRESSION

  ! Function result
  INTEGER(KIND=JPIB_K) :: IBITS_PER_VALUE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! NGRBCC   - 248 Cloud cover
  IF( KGRIBID .EQ. NGRBCC ) THEN
    IBITS_PER_VALUE = 8

  ! NGRBSD   - 141 Snow depth
  ! NGRBFSR  - 244 Forecast surface roughness
  ELSEIF ( KGRIBID .EQ. NGRBSD .OR. &
  &        KGRIBID .EQ. NGRBFSR ) THEN
    IBITS_PER_VALUE = 24


  ! NGRBCLWC - 246 Cloud liquid water content
  ELSEIF (  KGRIBID.EQ.NGRBCLWC .AND. KPREFIX.EQ.PRESSURE_LEVEL_E ) THEN
    IBITS_PER_VALUE = 12


  ! NGRBCIWC - 247 Cloud ice water content
  ELSEIF ( KGRIBID.EQ.NGRBCIWC  .AND. KPREFIX.EQ.PRESSURE_LEVEL_E ) THEN
    IBITS_PER_VALUE = 12


  ELSEIF ( KGRIBID .GT. 210000 .AND. &
&          KGRIBID .LT. 228000 )  THEN
    IBITS_PER_VALUE = 24

  ! NGRBCLBT - 260510 Cloudy brightness temperature
  ! NGRBCSBT - 260511 Clear-sky brightness temperature
  ELSEIF ( KGRIBID .EQ. NGRBCLBT .OR. &
&          KGRIBID .EQ. NGRBCSBT ) THEN
    IBITS_PER_VALUE = 10

  ELSEIF ( ENABLE_COMPRESSION .AND. &
&           KPREFIX.EQ.MODEL_LEVEL_E ) THEN
    IBITS_PER_VALUE = 10

  ELSE
    IBITS_PER_VALUE = 16


  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'COMPR_FC_GP_ML env. variable name of zero length' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'COMPR_FC_GP_ML env. variable name too long' )
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

END FUNCTION LOOKUP_BITS_PER_VALUE_DEFAULT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE PACKAGING_ASSUMPTIONS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME