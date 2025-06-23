#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


! Definition of the module
#define PP_FILE_NAME 'cgrib_api_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'CGRIB_API_MOD'
MODULE CGRIB_API_MOD

IMPLICIT NONE

grib_init_from_sample_name
grib_clone
grib_release
grib_set_missing
grib_is_defined
grib_set_string
grib_set_int
grib_set_long
grib_set_real4
grib_set_real8
grib_set_int_array
grib_set_long_array
grib_set_real4_array
grib_set_real8_array
grib_open_file
grib_close_file
grib_write
grib_get_message_size
grib_get_long


CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_API_INIT_FROM_SAMPLE_NAME'
PP_THREAD_SAFE FUNCTION CGRIB_API_INIT_FROM_SAMPLE_NAME( GRIB_HANDLE, SAMPLE_NAME, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_DOUBLE
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T

  ! Symbols imported from external libraries
  USE :: GRIB_API,    ONLY: GRIB_GET
  USE :: GRIB_API,    ONLY: GRIB_SET
  USE :: GRIB_API,    ONLY: GRIB_OPEN_FILE
  USE :: GRIB_API,    ONLY: GRIB_CLOSE_FILE
  USE :: GRIB_API,    ONLY: GRIB_NEW_FROM_SAMPLES
  USE :: GRIB_API,    ONLY: GRIB_SUCCESS
  USE :: GRIB_API,    ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(CGRIB_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),        INTENT(IN)    :: SAMPLE_NAME
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(LEN(SAMPLE_NAME)+1), TARGET :: C_SAMPLE_NAME
  TYPE(C_PTR) :: C_GRIB_HANDLE
  TYPE(C_PTR) :: C_SAMPLE_NAME_P
  INTEGER(KIND=JPIM_K) :: KRET
  INTEGER(KIND=C_LONG) :: NVALUES
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIM_K) :: SAMPLEFILE
  INTEGER(KIND=JPIM_K) :: I
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  CHARACTER(LEN=512) :: IOMSG
  REAL(KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: DUMMY_FIELD

  !> Local erro codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_NEW_FROM_SAMPLES_FAILED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_GET_FAILED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_FAILED=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_SET_FAILED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATE_FAILED=6_JPIB_K


  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  INTERFACE
    FUNCTION CGRIB_HANDLE_NEW_FROM_SAMPLES( CGRIB_CONTEXT, CSAMPLE_NAME ) &
&      RESULT(CGRIB_HANDLE) BIND(C, NAME='grib_handle_new_from_samples')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: CGRIB_CONTEXT
      TYPE(C_PTR), VALUE, INTENT(IN) :: CSAMPLE_NAME
      TYPE(C_PTR) :: CGRIB_HANDLE
    END FUNCTION CGRIB_HANDLE_NEW_FROM_SAMPLES
  END INTERFACE

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! This procedure can be called only if the object is not initialized
  PP_DEBUG_DEVELOP_COND_THROW( THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )

  CALL GRIB_NEW_FROM_SAMPLES( THIS%IGRIB_HANDLE_, TRIM(ADJUSTL(SAMPLE_NAME)), STATUS=KRET )

  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, ERRFLAG_GRIB_NEW_FROM_SAMPLES_FAILED )

  ! Set the grib handle
  DO I = 1, LEN(C_SAMPLE_NAME)
    C_SAMPLE_NAME(I:I) = SAMPLE_NAME(I:I)
  ENDDO
  I = LEN(C_SAMPLE_NAME) + 1
  C_SAMPLE_NAME(I:I) = C_NULL_CHAR
  C_SAMPLE_NAME_P = C_LOC(C_SAMPLE_NAME)
  C_GRIB_HANDLE = C_NULL_PTR
  C_GRIB_HANDLE = CGRIB_HANDLE_NEW_FROM_SAMPLES( C_NULL_PTR, C_SAMPLE_NAME_P )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(C_GRIB_HANDLE), ERRFLAG_GRIB_NEW_FROM_SAMPLES_FAILED )


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
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle already initialized' )
    CASE (ERRFLAG_GRIB_NEW_FROM_SAMPLES_FAILED)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to load the sample.' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'sample name: '//TRIM(ADJUSTL(SAMPLE_NAME)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'grib error: '//TRIM(ADJUSTL(GRIB_ERROR)) )
    CASE (ERRFLAG_GRIB_GET_FAILED)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read "numberOfValues" the sample.' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'grib error: '//TRIM(ADJUSTL(GRIB_ERROR)) )
    CASE (ERRFLAG_ALLOCATE_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating space for temporary buffer' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME(  TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_GRIB_SET_FAILED)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set "values".' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'grib error: '//TRIM(ADJUSTL(GRIB_ERROR)) )
    CASE (ERRFLAG_DEALLOCATE_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating temporary buffer -> '//TRIM(ADJUSTL(ERRMSG)) )
      DEALLOCATE(ERRMSG)
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION CGRIB_API_INIT_FROM_SAMPLE_NAME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



END MODULE CGRIB_API_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
