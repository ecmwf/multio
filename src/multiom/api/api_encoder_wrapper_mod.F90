! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'api_encoder_wrapper_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'API_ENCODER_WRAPPER_MOD'
MODULE API_ENCODER_WRAPPER_MOD

  ! USE :: ENCODERS_MAP_MOD, ONLY: ENCODERS_MAP_T
  ! USE :: ENCODERS_MAP_MOD, ONLY: MARS_MAP_T
  ! USE :: ENCODERS_MAP_MOD, ONLY: PAR_MAP_T

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

! In order to exchange data with c, we need to define the following maps
! TYPE(ENCODERS_MAP_T) :: ENCODERS_MAP
! TYPE(ENCODERS_MAP_T) :: MARS_MAP
! TYPE(ENCODERS_MAP_T) :: PAR_MAP


! Whitelist of public symbols (encoder management)
PUBLIC :: MULTIO_GRIB2_ENCODER_OPEN
PUBLIC :: MULTIO_GRIB2_ENCODER_CLOSE
PUBLIC :: MULTIO_GRIB2_ENCODER_ENCODE64
PUBLIC :: MULTIO_GRIB2_ENCODER_ENCODE32
PUBLIC :: MULTIO_GRIB2_ENCODER_EXTRACT_METADATA

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB2_ENCODER_OPEN'
PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_ENCODER_OPEN( OPTIONS, MULTIO_GRIB2 ) &
 BIND(C,NAME='multio_grib2_encoder_open') RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR), VALUE, INTENT(IN)    :: OPTIONS
  TYPE(C_PTR),        INTENT(INOUT) :: MULTIO_GRIB2

  !> Function result
  INTEGER(KIND=C_INT) :: RET

  !> Local variables
  LOGICAL :: OPTIONS_PRESENT

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_ALREADY_ASSOCIATED=1_JPIB_K

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

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( C_ASSOCIATED(MULTIO_GRIB2), ERRFLAG_ENCODER_ALREADY_ASSOCIATED )

  !> Check if the options are present
  OPTIONS_PRESENT = .NOT.C_ASSOCIATED( OPTIONS )

  

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_SET_ERR_FAILURE( RET )

  ! TODO: Add error handling code here

  RETURN

END FUNCTION MULTIO_GRIB2_ENCODER_OPEN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB2_ENCODER_CLOSE'
PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_ENCODER_CLOSE( MULTIO_GRIB2 ) &
 BIND(C,NAME='multio_grib2_encoder_close') RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR), VALUE, INTENT(IN) :: MULTIO_GRIB2

  !> Function result
  INTEGER(KIND=C_INT) :: RET

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

  ! TODO: Add error handling code here

  RETURN

END FUNCTION MULTIO_GRIB2_ENCODER_CLOSE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB2_ENCODER_EXTRACT_METADATA'
PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_ENCODER_EXTRACT_METADATA( MULTIO_GRIB2, GRIB_HANDLE, MARS_DICT, PAR_DICT ) &
 BIND(C,NAME='multio_grib2_encoder_extract_metadata') RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR), VALUE, INTENT(IN)    :: MULTIO_GRIB2
  TYPE(C_PTR), VALUE, INTENT(IN)    :: GRIB_HANDLE
  TYPE(C_PTR),        INTENT(INOUT) :: MARS_DICT
  TYPE(C_PTR),        INTENT(INOUT) :: PAR_DICT

  !> Function result
  INTEGER(KIND=C_INT) :: RET

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

  ! TODO: Add error handling code here

  RETURN

END FUNCTION MULTIO_GRIB2_ENCODER_EXTRACT_METADATA
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB2_ENCODER_ENCODE64'
PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_ENCODER_ENCODE64( MULTIO_GRIB2, MARS_DICT, PAR_DICT, VALUES, DATA_LEN, GRIB_HANDLE ) &
 BIND(C,NAME='multio_grib2_encoder_encode64') RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_DOUBLE
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR),         VALUE, INTENT(IN) :: MULTIO_GRIB2
  TYPE(C_PTR),         VALUE, INTENT(IN) :: MARS_DICT
  TYPE(C_PTR),         VALUE, INTENT(IN) :: PAR_DICT
  TYPE(C_PTR),         VALUE, INTENT(IN) :: VALUES
  INTEGER(KIND=C_INT), VALUE, INTENT(IN) :: DATA_LEN
  TYPE(C_PTR),         VALUE, INTENT(IN) :: GRIB_HANDLE

  !> Function result
  INTEGER(KIND=C_INT) :: RET

  !> Local variables
  REAL(KIND=C_DOUBLE), DIMENSION(:), POINTER :: VALUES_ARRAY32
  INTEGER(KIND=C_LONG_LONG), POINTER :: ENCODER_ID
  INTEGER(KIND=C_LONG_LONG), POINTER :: MARS_ID
  INTEGER(KIND=C_LONG_LONG), POINTER :: PAR_ID

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_DATA_LEN=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUES_IS_NULL_POINTER=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIO_GRIB2_IS_NULL_POINTER=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MARS_DICT_IS_NULL_POINTER=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PAR_DICT_IS_NULL_POINTER=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_HANDLE_IS_NULL_POINTER=6_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  RET = 0_C_INT

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( DATA_LEN .LE. 0, ERRFLAG_WRONG_DATA_LEN )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(VALUES), ERRFLAG_VALUES_IS_NULL_POINTER )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(MULTIO_GRIB2), ERRFLAG_MULTIO_GRIB2_IS_NULL_POINTER )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(MARS_DICT), ERRFLAG_MARS_DICT_IS_NULL_POINTER )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(PAR_DICT), ERRFLAG_PAR_DICT_IS_NULL_POINTER )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(GRIB_HANDLE), ERRFLAG_GRIB_HANDLE_IS_NULL_POINTER )

  !> Cast the VALUES pointer to a real array
  CALL C_F_POINTER( VALUES, VALUES_ARRAY32, [DATA_LEN] )

  !> Get encoder handle from the c pointer
  CALL C_F_POINTER( MULTIO_GRIB2, ENCODER_ID )

  !> Get mars dictionary handle from the c pointer
  CALL C_F_POINTER( MARS_DICT, MARS_ID )

  !> Parameterization handle from the c pointer
  CALL C_F_POINTER( PAR_DICT, PAR_ID )

  !> 

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_SET_ERR_FAILURE( RET )

  ! TODO: Add error handling code here

  RETURN

END FUNCTION MULTIO_GRIB2_ENCODER_ENCODE64
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB2_ENCODER_ENCODE32'
PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_ENCODER_ENCODE32( MULTIO_GRIB2, MARS_DICT, PAR_DICT, VALUES, DATA_LEN, GRIB_HANDLE ) &
 BIND(C,NAME='multio_grib2_encoder_encode32') RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_FLOAT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR),         VALUE, INTENT(IN) :: MULTIO_GRIB2
  TYPE(C_PTR),         VALUE, INTENT(IN) :: MARS_DICT
  TYPE(C_PTR),         VALUE, INTENT(IN) :: PAR_DICT
  TYPE(C_PTR),         VALUE, INTENT(IN) :: VALUES
  INTEGER(KIND=C_INT), VALUE, INTENT(IN) :: DATA_LEN
  TYPE(C_PTR),         VALUE, INTENT(IN) :: GRIB_HANDLE

  !> Function result
  INTEGER(KIND=C_INT) :: RET

  !> Local variables
  REAL(KIND=C_FLOAT), DIMENSION(:), POINTER :: VALUES_ARRAY32
  INTEGER(KIND=C_LONG_LONG), POINTER :: ENCODER_ID
  INTEGER(KIND=C_LONG_LONG), POINTER :: MARS_ID
  INTEGER(KIND=C_LONG_LONG), POINTER :: PAR_ID

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_DATA_LEN=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUES_IS_NULL_POINTER=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIO_GRIB2_IS_NULL_POINTER=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MARS_DICT_IS_NULL_POINTER=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PAR_DICT_IS_NULL_POINTER=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_HANDLE_IS_NULL_POINTER=6_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  RET = 0_C_INT

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( DATA_LEN .LE. 0, ERRFLAG_WRONG_DATA_LEN )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(VALUES), ERRFLAG_VALUES_IS_NULL_POINTER )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(MULTIO_GRIB2), ERRFLAG_MULTIO_GRIB2_IS_NULL_POINTER )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(MARS_DICT), ERRFLAG_MARS_DICT_IS_NULL_POINTER )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(PAR_DICT), ERRFLAG_PAR_DICT_IS_NULL_POINTER )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(GRIB_HANDLE), ERRFLAG_GRIB_HANDLE_IS_NULL_POINTER )

  !> Cast the VALUES pointer to a real array
  CALL C_F_POINTER( VALUES, VALUES_ARRAY32, [DATA_LEN] )

  !> Get encoder handle from the c pointer
  CALL C_F_POINTER( MULTIO_GRIB2, ENCODER_ID )

  !> Get mars dictionary handle from the c pointer
  CALL C_F_POINTER( MARS_DICT, MARS_ID )

  !> Parameterization handle from the c pointer
  CALL C_F_POINTER( PAR_DICT, PAR_ID )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_SET_ERR_FAILURE( RET )

  ! TODO: Add error handling code here

  RETURN

END FUNCTION MULTIO_GRIB2_ENCODER_ENCODE32
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE API_ENCODER_WRAPPER_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME