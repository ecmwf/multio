! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'grib_encoder_indexer_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB_ENCODER_INDEXER_MOD'
MODULE GRIB_ENCODER_INDEXER_MOD

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

IMPLICIT NONE

! Default visibility
PRIVATE

INTEGER(KIND=JPIB_K), PARAMETER :: NPPREFIXES=200_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: NPREPRES=10_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: NPEDITIONS=5_JPIB_K


! GEneric interface to the hash generator
INTERFACE ENCODER_IDX
  MODULE PROCEDURE ENCODER_ID_ATM_GM
  MODULE PROCEDURE ENCODER_ID_WAM_GM
  MODULE PROCEDURE ENCODER_ID_PRE
END INTERFACE


! Whitelist of public symbols
PUBLIC :: ENCODERS_HASH_SIZE
PUBLIC :: ENCODER_IDX

CONTAINS

!>
!> @brief This function returns the size of the hast table to be allocated
!>
!> @result Hash table size
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODERS_HASH_SIZE'
FUNCTION ENCODERS_HASH_SIZE( ) RESULT(NSIZE)

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Function result
  INTEGER(KIND=JPIB_K) :: NSIZE

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  NSIZE = NPPREFIXES*NPREPRES*NPEDITIONS

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION ENCODERS_HASH_SIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Hash function used to retrieve the index of an encoder
!>
!> This hash function is employed to obtain the index of an encoder based on the
!> provided instance of the I/O server, an I/O server request, and the required
!> grib edition.
!>
!> @param [in] GRIB_INFO All the "static" information related to the field to be encoded
!> @param [in] MSG       Message to be encoded
!>
!> @result The index of the encoder
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODER_ID_ATM_GM'
FUNCTION ENCODER_ID_ATM_GM( GRIB_INFO, MSG ) RESULT(IDX)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(GRIB_INFO_T),  INTENT(IN) :: GRIB_INFO
  TYPE(OM_ATM_MSG_T), INTENT(IN) :: MSG

  ! Function result
  INTEGER(KIND=JPIB_K) :: IDX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( MSG%IPREF_.LT.LBOUND(GRIB_INFO%REQUESTED_ENCODING_,1), 1 )
  PP_DEBUG_DEVELOP_COND_THROW( MSG%IPREF_.GT.UBOUND(GRIB_INFO%REQUESTED_ENCODING_,1), 2 )

  ! Create the index of the encoder to be used
  IDX = ENCODER_ID_PRE( MSG%IPREF_, MSG%IREPRES_, GRIB_INFO%REQUESTED_ENCODING_(MSG%IPREF_) )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Index lower than lower bound' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Index greater than upper bound' )
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


END FUNCTION ENCODER_ID_ATM_GM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE
!>
!> @brief Hash function used to retrieve the index of an encoder
!>
!> This hash function is employed to obtain the index of an encoder based on the
!> provided instance of the I/O server, an I/O server request, and the required
!> grib edition.
!>
!> @param [in] GRIB_INFO All the "static" information related to the field to be encoded
!> @param [in] MSG       Message to be encoded
!>
!> @result The index of the encoder
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODER_ID_WAM_GM'
FUNCTION ENCODER_ID_WAM_GM( GRIB_INFO, MSG ) RESULT(IDX)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: OM_CORE_MOD,        ONLY: OM_WAM_MSG_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(GRIB_INFO_T),  INTENT(IN) :: GRIB_INFO
  TYPE(OM_WAM_MSG_T), INTENT(IN) :: MSG

  ! Function result
  INTEGER(KIND=JPIB_K) :: IDX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( MSG%IPREF_.LT.LBOUND(GRIB_INFO%REQUESTED_ENCODING_,1), 1 )
  PP_DEBUG_DEVELOP_COND_THROW( MSG%IPREF_.GT.UBOUND(GRIB_INFO%REQUESTED_ENCODING_,1), 2 )

  ! Create the index of the encoder to be used
  IDX = ENCODER_ID_PRE( MSG%IPREF_, MSG%IREPRES_, GRIB_INFO%REQUESTED_ENCODING_(MSG%IPREF_) )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Index lower than lower bound' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Index greater than upper bound' )
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

END FUNCTION ENCODER_ID_WAM_GM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Hash function used to retrieve the index of an encoder
!>
!> This hash function is employed to obtain the index of an encoder based on the
!> provided instance of the I/O server, an I/O server request, and the required
!> grib edition.
!>
!> @param [in] GRIB_INFO All the "static" information related to the field to be encoded
!> @param [in] MSG       Message to be encoded
!>
!> @result The index of the encoder
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODER_ID_PRE'
FUNCTION ENCODER_ID_PRE( KPREFIX, KREPRES, KEDITION ) RESULT(IDX)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: OM_CORE_MOD,        ONLY: OM_BASE_MSG_A
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: OM_CORE_MOD,        ONLY: OM_WAM_MSG_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: KPREFIX
  INTEGER(KIND=JPIB_K), INTENT(IN) :: KREPRES
  INTEGER(KIND=JPIB_K), INTENT(IN) :: KEDITION

  ! Function result
  INTEGER(KIND=JPIB_K) :: IDX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( KPREFIX.LE.0, 1 )
  PP_DEBUG_DEVELOP_COND_THROW( KREPRES.LE.0, 2 )
  PP_DEBUG_DEVELOP_COND_THROW( KEDITION.LE.0, 3 )

  PP_DEBUG_DEVELOP_COND_THROW( KPREFIX.GT.NPPREFIXES, 4 )
  PP_DEBUG_DEVELOP_COND_THROW( KREPRES.GT.NPREPRES, 5 )
  PP_DEBUG_DEVELOP_COND_THROW( KEDITION.GT.NPEDITIONS, 6 )

  ! Compute the index
  IDX = (KEDITION-1)*(NPPREFIXES*NPREPRES) + (KREPRES-1)*NPPREFIXES + KPREFIX

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Index out of bunds. KPREFIX lower than 1' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Index out of bunds. KREPRES lower than 1' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Index out of bunds. KEDITION lower than 1' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Index out of bunds. KPREFIX higher than NPPREFIXES' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Index out of bunds. KREPRES higher than NPREPRES' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Index out of bunds. KEDITION higher than NPEDITIONS' )
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

END FUNCTION ENCODER_ID_PRE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE GRIB_ENCODER_INDEXER_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME