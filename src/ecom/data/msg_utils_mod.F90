!> @file om_message_mod.F90
!>
!> @brief Definition of a message for the output manager
!>
!> This module defines the message used by the output manager
!> and some utitlities to handle these messages
!>
!> @author Mirco Valentini
!> @date February 10, 2024

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


! Definition of the module
#define PP_FILE_NAME 'msg_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'MSG_UTILS_MOD'
MODULE MSG_UTILS_MOD

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPRB_K

IMPLICIT NONE

PRIVATE

! Whitelist of public symbols
PUBLIC :: CPREF2IPREF
PUBLIC :: IPREF2MSGTYPE
PUBLIC :: IPREFIX2ILEVTYPE
PUBLIC :: CLEVTYPE2ILEVTYPE
PUBLIC :: ILEVTYPE2CLEVTYPE
PUBLIC :: IREPRES2CREPRES


PUBLIC :: MSG_CREATE_NAME
PUBLIC :: MSG_WOPEN
PUBLIC :: MSG_ROPEN
PUBLIC :: MSG_CLOSE
PUBLIC :: MSG_PRINT_ATM
PUBLIC :: MSG_PRINT_WAM
PUBLIC :: MSG_WRITE_ATM
PUBLIC :: MSG_WRITE_WAM
PUBLIC :: MSG_READ_ATM
PUBLIC :: MSG_READ_WAM

CONTAINS



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IPREF2MSGTYPE'
FUNCTION IPREF2MSGTYPE( KPREF ) RESULT(MSGTYPE)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: ATM_MSG_E
  USE :: OM_CORE_MOD, ONLY: WAM_MSG_E
  USE :: OM_CORE_MOD, ONLY: MODEL_LEVEL_E
  USE :: OM_CORE_MOD, ONLY: PRESSURE_LEVEL_E
  USE :: OM_CORE_MOD, ONLY: VORTICITY_LEVEL_E
  USE :: OM_CORE_MOD, ONLY: THETA_LEVEL_E
  USE :: OM_CORE_MOD, ONLY: SURFACE_E
  USE :: OM_CORE_MOD, ONLY: WAVE_INT_E
  USE :: OM_CORE_MOD, ONLY: WAVE_SPEC_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: KPREF

  ! Function result
  INTEGER(KIND=JPIB_K) :: MSGTYPE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  SELECT CASE( KPREF )
  CASE ( MODEL_LEVEL_E, PRESSURE_LEVEL_E, VORTICITY_LEVEL_E, THETA_LEVEL_E, SURFACE_E  )
    MSGTYPE = ATM_MSG_E
  CASE ( WAVE_INT_E, WAVE_SPEC_E )
    MSGTYPE = WAM_MSG_E
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

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'unknown prefix' )

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

END FUNCTION IPREF2MSGTYPE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CPREF2IPREF'
FUNCTION CPREF2IPREF( CDPREF ) RESULT(IPREF)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: MODEL_LEVEL_E
  USE :: OM_CORE_MOD, ONLY: PRESSURE_LEVEL_E
  USE :: OM_CORE_MOD, ONLY: VORTICITY_LEVEL_E
  USE :: OM_CORE_MOD, ONLY: THETA_LEVEL_E
  USE :: OM_CORE_MOD, ONLY: SURFACE_E
  USE :: OM_CORE_MOD, ONLY: WAVE_INT_E
  USE :: OM_CORE_MOD, ONLY: WAVE_SPEC_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*), INTENT(IN) :: CDPREF

  ! Function result
  INTEGER(KIND=JPIB_K) :: IPREF

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling TODO: This check is not ok
  ! PP_DEBUG_DEVELOP_COND_THROW( LEN(CDPREF).LT.7,  1 )

  IF(CDPREF(1:2) == 'SF' .OR. CDPREF(1:1) == 's' .OR. CDPREF(1:3) == 'sfc') THEN
    IPREF = SURFACE_E
  ELSEIF(CDPREF(1:2) == 'ML' .OR. CDPREF(1:1) == 'm') THEN
    IPREF = MODEL_LEVEL_E
  ELSEIF(CDPREF(1:2) == 'PL' .OR. CDPREF(1:1) == 'p') THEN
    IPREF = PRESSURE_LEVEL_E
  ELSEIF(CDPREF(1:2) == 'PV' .OR. CDPREF(1:1) == 'v') THEN
    IPREF = VORTICITY_LEVEL_E
  ELSEIF(CDPREF(1:2) == 'TH' .OR. CDPREF(1:1) == 't') THEN
    IPREF = THETA_LEVEL_E
  ELSEIF(CDPREF(1:6) == 'wv_int') THEN
    IPREF = WAVE_INT_E
  ELSEIF(CDPREF(1:7) == 'wv_spec') THEN
    IPREF = WAVE_SPEC_E
  ELSE
    PP_DEBUG_DEVELOP_THROW( 2 )
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

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'prefix name too short' )

    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'unknown prefix: '//TRIM(CDPREF) )

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

END FUNCTION CPREF2IPREF
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'IPREFIX2ILEVTYPE'
FUNCTION IPREFIX2ILEVTYPE( IPREFIX, PARAM_ID, LEVEL, REPRES ) RESULT(ILEVTYPE)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  USE :: GRIB_CODES_MOD, ONLY: NGRBRSN
  USE :: GRIB_CODES_MOD, ONLY: NGRBTSN
  USE :: GRIB_CODES_MOD, ONLY: NGRBWSN
  USE :: GRIB_CODES_MOD, ONLY: NGRBSD
  USE :: GRIB_CODES_MOD, ONLY: NGRB100U
  USE :: GRIB_CODES_MOD, ONLY: NGRB100V

  USE :: OM_CORE_MOD, ONLY: MODEL_LEVEL_E
  USE :: OM_CORE_MOD, ONLY: PRESSURE_LEVEL_E
  USE :: OM_CORE_MOD, ONLY: VORTICITY_LEVEL_E
  USE :: OM_CORE_MOD, ONLY: THETA_LEVEL_E
  USE :: OM_CORE_MOD, ONLY: SURFACE_E
  USE :: OM_CORE_MOD, ONLY: WAVE_INT_E
  USE :: OM_CORE_MOD, ONLY: WAVE_SPEC_E

  USE :: OM_CORE_MOD, ONLY: LEVTYPE_HHL_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_HPL_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_HL_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_ML_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_O2D_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_O3D_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_PL_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_PT_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_PV_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_SFC_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_SOL_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K) , INTENT(IN) :: IPREFIX
  INTEGER(KIND=JPIB_K) , INTENT(IN) :: PARAM_ID
  INTEGER(KIND=JPIB_K) , INTENT(IN) :: LEVEL
  INTEGER(KIND=JPIB_K) , INTENT(IN) :: REPRES

  ! Function result
  INTEGER(KIND=JPIB_K) :: ILEVTYPE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! NGRBRSN  - 33     - Snow density
  ! NGRBTSN  - 238    - Temperature of snow layer
  ! NGRBWSN  - 228038 - Snow liquid water (multi-layer)
  ! NGRBSD   - 228141 - Snow depth (multi-layer)

  SELECT CASE ( IPREFIX )

  CASE ( MODEL_LEVEL_E )
    SELECT CASE (PARAM_ID)
    CASE ( NGRB100U, NGRB100V )
      ILEVTYPE = LEVTYPE_HL_E
    CASE DEFAULT
      ILEVTYPE = LEVTYPE_ML_E
    END SELECT
  CASE ( PRESSURE_LEVEL_E )
    SELECT CASE (PARAM_ID)
    CASE ( NGRB100U, NGRB100V )
      ILEVTYPE = LEVTYPE_HL_E
    CASE DEFAULT
      ILEVTYPE = LEVTYPE_PL_E
    END SELECT
  CASE ( VORTICITY_LEVEL_E )
    ILEVTYPE = LEVTYPE_PV_E
  CASE ( THETA_LEVEL_E )
    ILEVTYPE = LEVTYPE_PT_E
  CASE ( SURFACE_E )
    SELECT CASE (PARAM_ID)
    CASE ( NGRBRSN, NGRBTSN, NGRBWSN, NGRBSD, 231027 )
      IF ( LEVEL .NE. 0 ) THEN
        ILEVTYPE = LEVTYPE_SOL_E
      ELSE
        ILEVTYPE = LEVTYPE_SFC_E
      END IF
    CASE DEFAULT
      ILEVTYPE = LEVTYPE_SFC_E
    END SELECT
  CASE ( WAVE_INT_E )
    ILEVTYPE = LEVTYPE_SFC_E
  CASE ( WAVE_SPEC_E )
    ILEVTYPE = LEVTYPE_SFC_E
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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown levtype' )
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

END FUNCTION IPREFIX2ILEVTYPE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'CLEVTYPE2ILEVTYPE'
FUNCTION CLEVTYPE2ILEVTYPE( CLEVTYPE ) RESULT(ILEVTYPE)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_HHL_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_HPL_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_HL_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_ML_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_O2D_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_O3D_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_PL_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_PT_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_PV_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_SFC_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_SOL_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*), INTENT(IN) :: CLEVTYPE

  ! Function result
  INTEGER(KIND=JPIB_K) :: ILEVTYPE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  SELECT CASE ( CLEVTYPE )

  CASE ( 'hhl' )
    ILEVTYPE = LEVTYPE_HHL_E
  CASE ( 'hpl' )
    ILEVTYPE = LEVTYPE_HPL_E
  CASE ( 'hl' )
    ILEVTYPE = LEVTYPE_HL_E
  CASE ( 'ml' )
    ILEVTYPE = LEVTYPE_ML_E
  CASE ( 'o2d' )
    ILEVTYPE = LEVTYPE_O2D_E
  CASE ( 'o3d' )
    ILEVTYPE = LEVTYPE_O3D_E
  CASE ( 'pl' )
    ILEVTYPE = LEVTYPE_PL_E
  CASE ( 'pt' )
    ILEVTYPE = LEVTYPE_PT_E
  CASE ( 'pv' )
    ILEVTYPE = LEVTYPE_PV_E
  CASE ( 'sfc' )
    ILEVTYPE = LEVTYPE_SFC_E
  CASE ( 'sol' )
    ILEVTYPE = LEVTYPE_SOL_E
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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown levtype' )
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

END FUNCTION CLEVTYPE2ILEVTYPE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ILEVTYPE2CLEVTYPE'
FUNCTION ILEVTYPE2CLEVTYPE( ILEVTYPE ) RESULT(CLEVTYPE)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_HHL_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_HPL_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_HL_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_ML_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_O2D_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_O3D_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_PL_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_PT_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_PV_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_SFC_E
  USE :: OM_CORE_MOD, ONLY: LEVTYPE_SOL_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: ILEVTYPE

  ! Function result
  CHARACTER(LEN=3) :: CLEVTYPE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  SELECT CASE ( ILEVTYPE )

  CASE ( LEVTYPE_HHL_E )
    CLEVTYPE = 'hhl'
  CASE ( LEVTYPE_HPL_E )
    CLEVTYPE = 'hpl'
  CASE ( LEVTYPE_HL_E )
    CLEVTYPE = 'hl'
  CASE ( LEVTYPE_ML_E )
    CLEVTYPE = 'ml'
  CASE ( LEVTYPE_O2D_E )
    CLEVTYPE = 'o2d'
  CASE ( LEVTYPE_O3D_E )
    CLEVTYPE = 'o3d'
  CASE ( LEVTYPE_PL_E )
    CLEVTYPE = 'pl'
  CASE ( LEVTYPE_PT_E )
    CLEVTYPE = 'pt'
  CASE ( LEVTYPE_PV_E )
    CLEVTYPE = 'pv'
  CASE ( LEVTYPE_SFC_E )
    CLEVTYPE = 'sfc'
  CASE ( LEVTYPE_SOL_E )
    CLEVTYPE = 'sol'
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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown levtype' )
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

END FUNCTION ILEVTYPE2CLEVTYPE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'IREPRES2CREPRES'
FUNCTION IREPRES2CREPRES( IREPRES ) RESULT(CREPRES)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: REPRES_GRIDDED_E
  USE :: OM_CORE_MOD, ONLY: REPRES_SPECTRAL_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: IREPRES

  ! Function result
  CHARACTER(LEN=20) :: CREPRES

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  SELECT CASE ( IREPRES )

  CASE ( REPRES_GRIDDED_E )
    CREPRES = 'gridded'
  CASE ( REPRES_SPECTRAL_E )
    CREPRES = 'spherical_harmonics'
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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown levtype' )
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

END FUNCTION IREPRES2CREPRES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MSG_CREATE_NAME'
SUBROUTINE MSG_CREATE_NAME( DIRECTORY, MSG_ID, PROC_ID, MSGFNAME )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)  :: DIRECTORY
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: MSG_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: PROC_ID
  CHARACTER(LEN=*),     INTENT(OUT) :: MSGFNAME

  ! Local variables
  LOGICAL :: MSGEXIST
  INTEGER(KIND=JPIB_K) :: N
  INTEGER(KIND=JPIB_K) :: M
  INTEGER(KIND=JPIB_K) :: STAT


  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  N = LEN(MSGFNAME)
  M = LEN_TRIM(DIRECTORY) + 26
  PP_DEBUG_CRITICAL_COND_THROW( M.GT.N, 1)

  ! Create the message name
  MSGFNAME = REPEAT(' ',N)
  WRITE(MSGFNAME,'(A,A,I8.8,A,I8.8,A)', IOSTAT=STAT) TRIM(ADJUSTL(DIRECTORY)), '/msg_', MSG_ID, '_', PROC_ID, '.bin'
  PP_DEBUG_CRITICAL_COND_THROW(STAT.NE.0, 2)

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Values file name variable too short' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create the msg file name' )
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

END SUBROUTINE MSG_CREATE_NAME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MSG_WOPEN'
SUBROUTINE MSG_WOPEN( MSGFNAME, MSGUNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: MSGFNAME
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: MSGUNIT

  ! Local variables
  LOGICAL :: MSGEXIST
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( FILE=TRIM(MSGFNAME), EXIST=MSGEXIST )
  PP_DEBUG_CRITICAL_COND_THROW( MSGEXIST, 1)

  ! Open the TOC file
  OPEN( NEWUNIT=MSGUNIT, FILE=TRIM(MSGFNAME), STATUS='REPLACE', ACCESS='STREAM', ACTION='WRITE', FORM='UNFORMATTED', IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Val file already exists' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to open msg file' )
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

END SUBROUTINE MSG_WOPEN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MSG_ROPEN'
SUBROUTINE MSG_ROPEN( MSGFNAME, MSGUNIT, BIG_ENDIAN_READ )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)  :: MSGFNAME
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: MSGUNIT
  LOGICAL,              INTENT(IN)  :: BIG_ENDIAN_READ

  ! Local variables
  LOGICAL :: MSGEXIST
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( FILE=TRIM(MSGFNAME), EXIST=MSGEXIST )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.MSGEXIST, 1)

  ! Open the TOC file
  IF ( BIG_ENDIAN_READ ) THEN
    OPEN( NEWUNIT=MSGUNIT, FILE=TRIM(MSGFNAME), STATUS='OLD', ACCESS='STREAM', ACTION='READ', FORM='UNFORMATTED', CONVERT='BIG_ENDIAN', IOSTAT=STAT )
  ELSE
    OPEN( NEWUNIT=MSGUNIT, FILE=TRIM(MSGFNAME), STATUS='OLD', ACCESS='STREAM', ACTION='READ', FORM='UNFORMATTED', IOSTAT=STAT )
  ENDIF
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to find msg file: '//TRIM(MSGFNAME) )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to open msg file: '//TRIM(MSGFNAME) )
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

END SUBROUTINE MSG_ROPEN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MSG_CLOSE'
SUBROUTINE MSG_CLOSE( MSGUNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: MSGUNIT

  ! Local variables
  LOGICAL :: MSGOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( UNIT=MSGUNIT, OPENED=MSGOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.MSGOPENED, 1)

  ! Open the TOC file
  CLOSE( UNIT=MSGUNIT, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'unit not connected to a msg file' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error closing msg file' )
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

END SUBROUTINE MSG_CLOSE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MSG_READ_ATM'
SUBROUTINE MSG_READ_ATM( MSGUNIT, MSG, ADDR, VERBOSE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: OM_ATM_MSG_T
  USE :: OM_CORE_MOD, ONLY: ATM_MSG_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: MSGUNIT
  TYPE(OM_ATM_MSG_T),   INTENT(OUT) :: MSG
  INTEGER(KIND=INT64),  INTENT(IN)  :: ADDR
  LOGICAL, OPTIONAL,    INTENT(IN)  :: VERBOSE

  ! Local variables
  LOGICAL :: MSGOPENED
  LOGICAL :: LOC_VERBOSE
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=INT64)  :: MSG_TYPE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialisation
  IF ( PRESENT(VERBOSE) ) THEN
    LOC_VERBOSE = VERBOSE
  ELSE
    LOC_VERBOSE = .FALSE.
  ENDIF

  ! Check that the file is opened
  INQUIRE( UNIT=MSGUNIT, OPENED=MSGOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.MSGOPENED, 1)

  ! Read the message type
  READ(MSGUNIT,POS=ADDR,IOSTAT=STAT ) MSG_TYPE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )
  PP_DEBUG_DEVELOP_COND_THROW( MSG_TYPE.NE.ATM_MSG_E, 3 )

  ! Allocate the proper message
  CALL READ_ATM(  MSG, MSGUNIT, LOC_VERBOSE )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Read unit not connected to a file' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read message type from file' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Invalid message type' )
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

END SUBROUTINE MSG_READ_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MSG_WRITE_ATM'
SUBROUTINE MSG_WRITE_ATM( UNIT, MSG, ADDR )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPRM_K
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: OM_ATM_MSG_T
  USE :: OM_CORE_MOD, ONLY: ATM_MSG_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: UNIT
  TYPE(OM_ATM_MSG_T),   INTENT(IN)  :: MSG
  INTEGER(KIND=INT64),  INTENT(OUT) :: ADDR

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()


  FLUSH(UNIT)

  ! Record the last position written
  INQUIRE( UNIT, POS=ADDR, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 4)

  WRITE(UNIT, IOSTAT=STAT) INT( ATM_MSG_E, INT64)
  FLUSH(UNIT)

  ! Write atmosphere
  CALL WRITE_ATM( MSG, UNIT )
  FLUSH(UNIT)

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown message type' )

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

END SUBROUTINE MSG_WRITE_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MSG_PRINT_ATM'
SUBROUTINE MSG_PRINT_ATM( MSG, UNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: OM_ATM_MSG_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_ATM_MSG_T),   INTENT(IN)  :: MSG
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: UNIT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Write wam
  CALL PRINT_ATM( MSG, UNIT )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MSG_PRINT_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MSG_READ_WAM'
SUBROUTINE MSG_READ_WAM( MSGUNIT, MSG, ADDR, VERBOSE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: OM_WAM_MSG_T
  USE :: OM_CORE_MOD, ONLY: WAM_MSG_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: MSGUNIT
  TYPE(OM_WAM_MSG_T),   INTENT(OUT) :: MSG
  INTEGER(KIND=INT64),  INTENT(IN)  :: ADDR
  LOGICAL, OPTIONAL,    INTENT(IN)  :: VERBOSE

  ! Local variables
  LOGICAL :: MSGOPENED
  LOGICAL :: LOC_VERBOSE
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=INT64)  :: MSG_TYPE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialisation
  IF ( PRESENT(VERBOSE) ) THEN
    LOC_VERBOSE = VERBOSE
  ELSE
    LOC_VERBOSE = .FALSE.
  ENDIF

  ! Check that the file is opened
  INQUIRE( UNIT=MSGUNIT, OPENED=MSGOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.MSGOPENED, 1)

  ! Read the message type
  READ(MSGUNIT,POS=ADDR,IOSTAT=STAT ) MSG_TYPE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )
  PP_DEBUG_DEVELOP_COND_THROW( MSG_TYPE.NE.WAM_MSG_E, 3 )

  ! Allocate the proper message
  CALL READ_WAM(  MSG, MSGUNIT, LOC_VERBOSE )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=16) :: TMP

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Read unit not connected to a file' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read message type from file' )
    CASE (3)
      WRITE(TMP,'(I10)') MSG_TYPE
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Invalid message type: '//TRIM(ADJUSTL(TMP)) )
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

END SUBROUTINE MSG_READ_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MSG_WRITE_WAM'
SUBROUTINE MSG_WRITE_WAM( UNIT, MSG, ADDR )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPRM_K
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: OM_WAM_MSG_T
  USE :: OM_CORE_MOD, ONLY: WAM_MSG_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: UNIT
  TYPE(OM_WAM_MSG_T),   INTENT(IN)  :: MSG
  INTEGER(KIND=INT64),  INTENT(OUT) :: ADDR

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  FLUSH(UNIT)

  ! Get the position inthe file
  INQUIRE( UNIT, POS=ADDR, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 4)

  ! Write the kind of message
  WRITE(UNIT, IOSTAT=STAT) INT( WAM_MSG_E, INT64)
  FLUSH(UNIT)

  ! Write wam
  CALL WRITE_WAM( MSG, UNIT )
  FLUSH(UNIT)

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown message type' )

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

END SUBROUTINE MSG_WRITE_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MSG_PRINT_WAM'
SUBROUTINE MSG_PRINT_WAM( MSG, UNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: OM_WAM_MSG_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_WAM_MSG_T),   INTENT(IN)  :: MSG
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: UNIT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Write wam
  ! CALL PRINT_BASE( MSG, UNIT )
  CALL PRINT_WAM( MSG, UNIT )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MSG_PRINT_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'WRITE_ATM'
SUBROUTINE WRITE_ATM( DATA, UNIT )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: OM_ATM_MSG_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_ATM_MSG_T),   INTENT(IN) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN) :: UNIT

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialisation
  STAT = -99

  ! Common data
  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IUID_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%PARAM_ID_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%ISTEP_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 3 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IPREF_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 4 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IREPRES_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 5 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NVALUES_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 6 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NUNDF_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 7 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%XUNDF_ , REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 8 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%MINVAL_ , REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 9 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%MAXVAL_ , REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 10 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%AVGVAL_ , REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 11 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%ZP_ , REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 12 )

  ! Data specific for atmosphere
  WRITE(UNIT,IOSTAT=STAT) INT( DATA%ILEVG_, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 13 )

  WRITE(UNIT,IOSTAT=STAT) INT( DATA%NGRIBL_, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 14 )

  WRITE(UNIT,IOSTAT=STAT) INT( DATA%IPREVPP_, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 15 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%IUID_\"' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%PARAM_ID_\"' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%ISTEP_\"' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%IPREF_\"' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%IREPRES_\"' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NVALUES_\"' )
    CASE (7)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%LUNDF_\"' )
    CASE (8)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%XUNDF_\"' )
    CASE (9)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%MINVAL_\"' )
    CASE (10)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%MAXVAL_\"' )
    CASE (11)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%AVGVAL_\"' )
    CASE (12)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%ZP_\"' )


    CASE (13)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \" DATA%ILEVG_\"' )
    CASE (14)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NGRIBL_\"' )
    CASE (15)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%IPREVPP_\"' )
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

END SUBROUTINE WRITE_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_ATM'
SUBROUTINE READ_ATM(DATA, UNIT, VERBOSE)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: OM_ATM_MSG_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_ATM_MSG_T),   INTENT(INOUT) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  LOGICAL,              INTENT(IN)    :: VERBOSE

  ! Local variables
  INTEGER(KIND=INT64) :: ITMP
  REAL(KIND=REAL64)   :: ZTMP
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Read base data
  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )
  DATA%IUID_ = INT( ITMP, KIND( DATA%IUID_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%IUID_................ :: ', DATA%IUID_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )
  DATA%PARAM_ID_ = INT( ITMP, KIND( DATA%PARAM_ID_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%PARAM_ID_............ :: ', DATA%PARAM_ID_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 3 )
  DATA%ISTEP_ = INT( ITMP, KIND( DATA%ISTEP_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%ISTEP_............... :: ', DATA%ISTEP_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 4 )
  DATA%IPREF_ = INT( ITMP, KIND( DATA%IPREF_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%IPREF_............... :: ', DATA%IPREF_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 5 )
  DATA%IREPRES_ = INT( ITMP, KIND( DATA%IREPRES_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%IREPRES_............. :: ', DATA%IREPRES_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 6 )
  DATA%NVALUES_ = INT( ITMP, KIND( DATA%NVALUES_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%NVALUES_............. :: ', DATA%NVALUES_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 7 )
  DATA%NUNDF_ = INT( ITMP, KIND( DATA%NUNDF_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%NUNDF_............... :: ', DATA%NUNDF_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ZTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 8 )
  DATA%XUNDF_ = REAL( ZTMP, KIND( DATA%XUNDF_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%XUNDF_............... :: ', DATA%XUNDF_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ZTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 9 )
  DATA%MINVAL_ = REAL( ZTMP, KIND( DATA%MINVAL_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%MINVAL_.............. :: ', DATA%MINVAL_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ZTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 10 )
  DATA%MAXVAL_ = REAL( ZTMP, KIND( DATA%MAXVAL_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%MAXVAL_.............. :: ', DATA%MAXVAL_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ZTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 11 )
  DATA%AVGVAL_ = REAL( ZTMP, KIND( DATA%AVGVAL_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%AVGVAL_.............. :: ', DATA%AVGVAL_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ZTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 12 )
  DATA%ZP_ = REAL( ZTMP, KIND( DATA%ZP_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%ZP_.................. :: ', DATA%ZP_
  ENDIF

  ! Specific data for atmosphere
  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 13 )
  DATA%ILEVG_ = INT( ITMP, KIND(DATA%ILEVG_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_ATM%ILEVG_................ :: ', DATA%ILEVG_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 14 )
  DATA%NGRIBL_ = INT( ITMP, KIND(DATA%NGRIBL_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_ATM%NGRIBL_............... :: ', DATA%NGRIBL_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 15 )
  DATA%IPREVPP_ = INT( ITMP, KIND(DATA%IPREVPP_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_ATM%IPREVPP_.............. :: ', DATA%IPREVPP_
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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%IUID_\"' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%PARAM_ID_\"' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%ISTEP_\"' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%IPREF_\"' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%IREPRES_\"' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%NVALUES_\"' )
    CASE (7)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%LUNDF_\"' )
    CASE (8)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%XUNDF_\"' )
    CASE (9)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%MINVAL_\"' )
    CASE (10)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%MAXVAL_\"' )
    CASE (11)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%AVGVAL_\"' )
    CASE (12)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%ZP_\"' )
    CASE (13)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \" DATA%ILEVG_\"' )
    CASE (14)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NGRIBL_\"' )
    CASE (15)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%IPREVPP_\"' )
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

END SUBROUTINE READ_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PRINT_ATM'
SUBROUTINE PRINT_ATM( DATA, UNIT )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: OM_ATM_MSG_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_ATM_MSG_T),   INTENT(IN) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN) :: UNIT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  WRITE(UNIT,*) ' + MSG_BASE%IUID_................ :: ', DATA%IUID_
  WRITE(UNIT,*) ' + MSG_BASE%PARAM_ID_............ :: ', DATA%PARAM_ID_
  WRITE(UNIT,*) ' + MSG_BASE%ISTEP_............... :: ', DATA%ISTEP_
  WRITE(UNIT,*) ' + MSG_BASE%IPREF_............... :: ', DATA%IPREF_
  WRITE(UNIT,*) ' + MSG_BASE%ILEVTYPE_............ :: ', IPREFIX2ILEVTYPE( DATA%IPREF_, DATA%PARAM_ID_, DATA%ILEVG_, DATA%IREPRES_ )
  WRITE(UNIT,*) ' + MSG_BASE%CLEVTYPE_............ :: ', ILEVTYPE2CLEVTYPE( IPREFIX2ILEVTYPE( DATA%IPREF_, DATA%PARAM_ID_, DATA%ILEVG_, DATA%IREPRES_ ) )
  WRITE(UNIT,*) ' + MSG_BASE%IREPRES_............. :: ', DATA%IREPRES_
  WRITE(UNIT,*) ' + MSG_BASE%CREPRES_............. :: ', IREPRES2CREPRES( DATA%IREPRES_ )
  WRITE(UNIT,*) ' + MSG_BASE%NVALUES_............. :: ', DATA%NVALUES_
  WRITE(UNIT,*) ' + MSG_BASE%NUNDF_............... :: ', DATA%NUNDF_
  WRITE(UNIT,*) ' + MSG_BASE%XUNDF_............... :: ', DATA%XUNDF_
  WRITE(UNIT,*) ' + MSG_BASE%MINVAL_.............. :: ', DATA%MINVAL_
  WRITE(UNIT,*) ' + MSG_BASE%MAXVAL_.............. :: ', DATA%MAXVAL_
  WRITE(UNIT,*) ' + MSG_BASE%AVGVAL_.............. :: ', DATA%AVGVAL_
  WRITE(UNIT,*) ' + MSG_BASE%ZPL_................. :: ', DATA%ZP_

  WRITE(UNIT,*) ' + MSG_ATM%ILEVG_................ :: ', DATA%ILEVG_
  WRITE(UNIT,*) ' + MSG_ATM%NGRIBL_............... :: ', DATA%NGRIBL_
  WRITE(UNIT,*) ' + MSG_ATM%IPREVPP_.............. :: ', DATA%IPREVPP_

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE PRINT_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'WRITE_WAM'
SUBROUTINE WRITE_WAM(DATA, UNIT)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: OM_WAM_MSG_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_WAM_MSG_T),   INTENT(IN) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN) :: UNIT

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialisation
  STAT = -99

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IUID_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%PARAM_ID_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%ISTEP_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 3 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IPREF_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 4 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IREPRES_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 5 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NVALUES_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 6 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NUNDF_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 7 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%XUNDF_ , REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 8 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%MINVAL_ , REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 9 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%MAXVAL_ , REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 10 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%AVGVAL_ , REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 11 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%ZP_ , REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 12 )


  ! Data specific for WAM
  WRITE(UNIT,IOSTAT=STAT) INT( DATA%IANGLE, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 13 )

  WRITE(UNIT,IOSTAT=STAT) INT( DATA%IFREQ, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 14 )

  WRITE(UNIT,IOSTAT=STAT) INT( DATA%NDATE_TIME_WINDOW_END, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 15 )

  WRITE(UNIT,IOSTAT=STAT) INT( DATA%KCOUSTEP, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 16 )

  WRITE(UNIT,IOSTAT=STAT) INT( DATA%ITABLE, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 17 )

  WRITE(UNIT,IOSTAT=STAT) INT( DATA%IPARAM, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 18 )

  WRITE(UNIT,IOSTAT=STAT) INT( DATA%KLEV, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 19 )

  WRITE(UNIT,IOSTAT=STAT) INT( DATA%IFCST, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 20 )

  WRITE(UNIT,IOSTAT=STAT) INT( DATA%NSTEP, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 21 )

  WRITE(UNIT,IOSTAT=STAT) DATA%LRSTST0
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 22 )

  WRITE(UNIT,IOSTAT=STAT) DATA%MARSTYPE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 23 )

  WRITE(UNIT,IOSTAT=STAT) DATA%CDATE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 24 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%IUID_\"' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%PARAM_ID_\"' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%ISTEP_\"' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%IPREF_\"' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%IREPRES_\"' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NVALUES_\"' )
    CASE (7)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%LUNDF_\"' )
    CASE (8)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%XUNDF_\"' )
    CASE (9)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%MINVAL_\"' )
    CASE (10)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%MAXVAL_\"' )
    CASE (11)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%AVGVAL_\"' )
    CASE (12)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%ZP_\"' )


    CASE (13)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%IANGLE\"' )
    CASE (14)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%IFREQ\"' )
    CASE (15)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NDATE_TIME_WINDOW_END\"' )
    CASE (16)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%KCOUSTEP\"' )
    CASE (17)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%ITABLE\"' )
    CASE (18)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%IPARAM\"' )
    CASE (19)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%KLEV\"' )
    CASE (20)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%IFCST\"' )
    CASE (21)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NSTEP\"' )
    CASE (22)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%LRSTST0\"' )
    CASE (23)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%MARSTYPE\"' )
    CASE (24)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%CDATE\"' )
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

END SUBROUTINE WRITE_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_WAM'
SUBROUTINE READ_WAM(DATA, UNIT, VERBOSE)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: OM_WAM_MSG_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_WAM_MSG_T),   INTENT(INOUT) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  LOGICAL,              INTENT(IN)    :: VERBOSE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=INT64)  :: ITMP
  REAL(KIND=REAL64)  :: ZTMP

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialisation
  STAT = -99

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )
  DATA%IUID_ = INT( ITMP, KIND( DATA%IUID_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%IUID_................ :: ', DATA%IUID_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )
  DATA%PARAM_ID_ = INT( ITMP, KIND( DATA%PARAM_ID_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%PARAM_ID_............ :: ', DATA%PARAM_ID_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 3 )
  DATA%ISTEP_ = INT( ITMP, KIND( DATA%ISTEP_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%ISTEP_............... :: ', DATA%ISTEP_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 4 )
  DATA%IPREF_ = INT( ITMP, KIND( DATA%IPREF_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%IPREF_............... :: ', DATA%IPREF_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 5 )
  DATA%IREPRES_ = INT( ITMP, KIND( DATA%IREPRES_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%IREPRES_............. :: ', DATA%IREPRES_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 6 )
  DATA%NVALUES_ = INT( ITMP, KIND( DATA%NVALUES_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%NVALUES_............. :: ', DATA%NVALUES_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 7 )
  DATA%NUNDF_ = INT( ITMP, KIND( DATA%NUNDF_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%NUNDF_............... :: ', DATA%NUNDF_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ZTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 8 )
  DATA%XUNDF_ = REAL( ZTMP, KIND( DATA%XUNDF_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%XUNDF_............... :: ', DATA%XUNDF_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ZTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 9 )
  DATA%MINVAL_ = REAL( ZTMP, KIND( DATA%MINVAL_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%MINVAL_.............. :: ', DATA%MINVAL_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ZTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 10 )
  DATA%MAXVAL_ = REAL( ZTMP, KIND( DATA%MAXVAL_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%MAXVAL_.............. :: ', DATA%MAXVAL_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ZTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 11 )
  DATA%AVGVAL_ = REAL( ZTMP, KIND( DATA%AVGVAL_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%AVGVAL_.............. :: ', DATA%AVGVAL_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ZTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 12 )
  DATA%ZP_ = REAL( ZTMP, KIND( DATA%ZP_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%ZP_.................. :: ', DATA%ZP_
  ENDIF



  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 13 )
  DATA%IANGLE = INT( ITMP, KIND(DATA%IANGLE) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%IANGLE................ :: ', DATA%IANGLE
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 14 )
  DATA%IFREQ = INT( ITMP, KIND(DATA%IFREQ) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%IFREQ................. :: ', DATA%IFREQ
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 15 )
  DATA%NDATE_TIME_WINDOW_END = INT( ITMP, KIND(DATA%NDATE_TIME_WINDOW_END) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%NDATE_TIME_WINDOW_END. :: ', DATA%NDATE_TIME_WINDOW_END
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 16 )
  DATA%KCOUSTEP = INT( ITMP, KIND(DATA%KCOUSTEP) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%KCOUSTEP.............. :: ', DATA%KCOUSTEP
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 17 )
  DATA%ITABLE = INT( ITMP, KIND(DATA%ITABLE) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%ITABLE................ :: ', DATA%ITABLE
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 18 )
  DATA%IPARAM = INT( ITMP, KIND(DATA%IPARAM) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%IPARAM................ :: ', DATA%IPARAM
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 19 )
  DATA%KLEV = INT( ITMP, KIND(DATA%KLEV) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%KLEV.................. :: ', DATA%KLEV
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 20 )
  DATA%IFCST = INT( ITMP, KIND(DATA%IFCST) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%IFCST................. :: ', DATA%IFCST
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 21 )
  DATA%NSTEP = INT( ITMP, KIND(DATA%NSTEP) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%NSTEP................. :: ', DATA%NSTEP
  ENDIF

  READ(UNIT,IOSTAT=STAT) DATA%LRSTST0
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 22 )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%LRSTST0............... :: ', DATA%LRSTST0
  ENDIF

  READ(UNIT,IOSTAT=STAT) DATA%MARSTYPE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 23 )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%MARSTYPE.............. :: ', DATA%MARSTYPE
  ENDIF

  READ(UNIT,IOSTAT=STAT) DATA%CDATE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 24 )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%CDATE................. :: ', DATA%CDATE
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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%IUID_\"' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%PARAM_ID_\"' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%ISTEP_\"' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%IPREF_\"' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%IREPRES_\"' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%NVALUES_\"' )
    CASE (7)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%LUNDF_\"' )
    CASE (8)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%XUNDF_\"' )
    CASE (9)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%MINVAL_\"' )
    CASE (10)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%MAXVAL_\"' )
    CASE (11)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%AVGVAL_\"' )
    CASE (12)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read : \"DATA%ZP_\"' )
    CASE (13)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%IANGLE\"' )
    CASE (14)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%IFREQ\"' )
    CASE (15)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NDATE_TIME_WINDOW_END\"' )
    CASE (16)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%KCOUSTEP\"' )
    CASE (17)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%ITABLE\"' )
    CASE (18)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%IPARAM\"' )
    CASE (19)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%KLEV\"' )
    CASE (20)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%IFCST\"' )
    CASE (21)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NSTEP\"' )
    CASE (22)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%LRSTST0\"' )
    CASE (23)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%MARSTYPE\"' )
    CASE (24)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%CDATE\"' )
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

END SUBROUTINE READ_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PRINT_WAM'
SUBROUTINE PRINT_WAM( DATA, UNIT )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: OM_WAM_MSG_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_WAM_MSG_T),   INTENT(IN) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN) :: UNIT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  WRITE(UNIT,*) ' + MSG_BASE%IUID_................ :: ', DATA%IUID_
  WRITE(UNIT,*) ' + MSG_BASE%PARAM_ID_............ :: ', DATA%PARAM_ID_
  WRITE(UNIT,*) ' + MSG_BASE%ISTEP_............... :: ', DATA%ISTEP_
  WRITE(UNIT,*) ' + MSG_BASE%IPREF_............... :: ', DATA%IPREF_
  WRITE(UNIT,*) ' + MSG_BASE%ILEVTYPE_............ :: ', IPREFIX2ILEVTYPE( DATA%IPREF_, DATA%PARAM_ID_, DATA%KLEV, DATA%IREPRES_ )
  WRITE(UNIT,*) ' + MSG_BASE%CLEVTYPE_............ :: ', ILEVTYPE2CLEVTYPE( IPREFIX2ILEVTYPE( DATA%IPREF_, DATA%PARAM_ID_, DATA%KLEV, DATA%IREPRES_ ) )
  WRITE(UNIT,*) ' + MSG_BASE%IREPRES_............. :: ', DATA%IREPRES_
  WRITE(UNIT,*) ' + MSG_BASE%CREPRES_............. :: ', IREPRES2CREPRES( DATA%IREPRES_ )
  WRITE(UNIT,*) ' + MSG_BASE%NVALUES_............. :: ', DATA%NVALUES_
  WRITE(UNIT,*) ' + MSG_BASE%NUNDF_............... :: ', DATA%NUNDF_
  WRITE(UNIT,*) ' + MSG_BASE%XUNDF_............... :: ', DATA%XUNDF_
  WRITE(UNIT,*) ' + MSG_BASE%MINVAL_.............. :: ', DATA%MINVAL_
  WRITE(UNIT,*) ' + MSG_BASE%MAXVAL_.............. :: ', DATA%MAXVAL_
  WRITE(UNIT,*) ' + MSG_BASE%AVGVAL_.............. :: ', DATA%AVGVAL_
  WRITE(UNIT,*) ' + MSG_BASE%ZPL_................. :: ', DATA%ZP_

  WRITE(UNIT,*) ' + MSG_WAM%IANGLE................ :: ', DATA%IANGLE
  WRITE(UNIT,*) ' + MSG_WAM%IFREQ................. :: ', DATA%IFREQ
  WRITE(UNIT,*) ' + MSG_WAM%NDATE_TIME_WINDOW_END. :: ', DATA%NDATE_TIME_WINDOW_END
  WRITE(UNIT,*) ' + MSG_WAM%KCOUSTEP.............. :: ', DATA%KCOUSTEP
  WRITE(UNIT,*) ' + MSG_WAM%ITABLE................ :: ', DATA%ITABLE
  WRITE(UNIT,*) ' + MSG_WAM%IPARAM................ :: ', DATA%IPARAM
  WRITE(UNIT,*) ' + MSG_WAM%KLEV.................. :: ', DATA%KLEV
  WRITE(UNIT,*) ' + MSG_WAM%IFCST................. :: ', DATA%IFCST
  WRITE(UNIT,*) ' + MSG_WAM%NSTEP................. :: ', DATA%NSTEP
  WRITE(UNIT,*) ' + MSG_WAM%LRSTST0............... :: ', DATA%LRSTST0
  WRITE(UNIT,*) ' + MSG_WAM%MARSTYPE.............. :: ', DATA%MARSTYPE
  WRITE(UNIT,*) ' + MSG_WAM%CDATE................. :: ', DATA%CDATE

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE PRINT_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE MSG_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME