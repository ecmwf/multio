! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'rules_api_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'RULES_API_MOD'
MODULE RULES_API_MOD

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

! Whitelist of public symbols (encoder management)
PUBLIC :: MULTIO_GRIB2_RULES_OPEN
PUBLIC :: MULTIO_GRIB2_RULES_CLOSE
PUBLIC :: MULTIO_GRIB2_RULES_SEARCH
PUBLIC :: MULTIO_GRIB2_RULES_PRINT

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB2_RULES_OPEN'
PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_RULES_OPEN( &
& C_OPTIONS_P, C_RULES_PP, C_RULES_FNAME_P, FLEN ) &
 BIND(C,NAME='multio_grib2_rules_open') RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T

  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: API_GENERAL_UTILS_MOD,              ONLY: DEREFERENCE_DOUBLE_C_POINTER
  USE :: API_GENERAL_UTILS_MOD,              ONLY: COPY_CPTR_TO_F_STRING
  USE :: API_OPTIONS_DICTIONARY_UTILS_MOD,   ONLY: API_OPTIONS_T
  USE :: API_OPTIONS_DICTIONARY_WRAPPER_MOD, ONLY: GET_API_OPTIONS_DICTIONARY
  USE :: FILTER_OPTIONS_MOD,                 ONLY: FILTER_OPTIONS_T
  USE :: RULES_WRAPPER_MOD,                  ONLY: INIT_RULES

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR),         VALUE, INTENT(IN) :: C_OPTIONS_P
  TYPE(C_PTR),         VALUE, INTENT(IN) :: C_RULES_PP
  TYPE(C_PTR),         VALUE, INTENT(IN) :: C_RULES_FNAME_P
  INTEGER(KIND=C_INT), VALUE, INTENT(IN) :: FLEN

  !> Function result
  INTEGER(KIND=C_INT) :: RET

  !> Local variables
  TYPE(HOOKS_T) :: HOOKS
  TYPE(FILTER_OPTIONS_T) :: FILTER_OPT
  TYPE(C_PTR), DIMENSION(:), POINTER :: C_RULES_P
  TYPE(API_OPTIONS_T) :: F_API_OPTIONS
  CHARACTER(LEN=1024) :: RULES_FNAME
  LOGICAL :: EXISTS

  !> Local error flags
  INTEGER(KIND=C_INT), PARAMETER :: ERRFLAG_DEREFERENCE_DOUBLE_POINTER = 1_JPIB_K
  INTEGER(KIND=C_INT), PARAMETER :: ERRFLAG_RULES_ALREADY_ASSOCIATED = 2_JPIB_K
  INTEGER(KIND=C_INT), PARAMETER :: ERRFLAG_UNABLE_TO_GET_OPTIONS_INFO = 3_JPIB_K
  INTEGER(KIND=C_INT), PARAMETER :: ERRFLAG_UNABLE_TO_READ_FNAME = 4_JPIB_K
  INTEGER(KIND=C_INT), PARAMETER :: ERRFLAG_RULES_FILE_DOES_NOT_EXIST = 5_JPIB_K
  INTEGER(KIND=C_INT), PARAMETER :: ERRFLAG_UNABLE_TO_INIT_RULES = 6_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )


  ! Initialization of the hooks
  CALL HOOKS%DEBUG_HOOK_%INIT( )

  !> Get the rules pointer
  C_RULES_P => NULL()
  PP_TRYCALL(ERRFLAG_DEREFERENCE_DOUBLE_POINTER) DEREFERENCE_DOUBLE_C_POINTER( &
&     C_RULES_PP, 1_JPIB_K, C_RULES_P, HOOKS )

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( C_ASSOCIATED(C_RULES_P(1)), ERRFLAG_RULES_ALREADY_ASSOCIATED )

  !> Get the encoder options
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_OPTIONS_INFO) GET_API_OPTIONS_DICTIONARY( &
&       C_OPTIONS_P, F_API_OPTIONS, HOOKS )

  !> Get the type of the encoder to be created
  RULES_FNAME = REPEAT(' ', LEN(RULES_FNAME))
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_FNAME) COPY_CPTR_TO_F_STRING( &
&        C_RULES_FNAME_P, INT(FLEN,C_SIZE_T), RULES_FNAME, HOOKS, TOLOWER=.FALSE. )

  !> Check if file exists
  INQUIRE( FILE=TRIM(ADJUSTL(RULES_FNAME)), EXIST=EXISTS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.EXISTS, ERRFLAG_RULES_FILE_DOES_NOT_EXIST )

  !> MIVAL TODO: FILTER_OPT = f(F_API_OPTIONS)

  !> Initialize the rules
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INIT_RULES) INIT_RULES( C_RULES_P(1), &
&    TRIM(ADJUSTL(RULES_FNAME)), FILTER_OPT, HOOKS )

  !> Be sure we don't have any memory leaks
  CALL HOOKS%DEBUG_HOOK_%FREE( )

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

    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_DEREFERENCE_DOUBLE_POINTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error dereferencing double pointer' )
    CASE (ERRFLAG_RULES_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Rules already associated' )
    CASE (ERRFLAG_UNABLE_TO_GET_OPTIONS_INFO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get options info' )
    CASE (ERRFLAG_UNABLE_TO_READ_FNAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read rules file name' )
    CASE (ERRFLAG_RULES_FILE_DOES_NOT_EXIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Rules file does not exist: "'//TRIM(RULES_FNAME)//'"' )
    CASE (ERRFLAG_UNABLE_TO_INIT_RULES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to initialize rules with file: "'//TRIM(RULES_FNAME)//'"' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown error' )
    END SELECT

    ! Print the error stack
    ! NOTE: This is important when c is calling this function.
    ! It opens the error_unit
    WRITE(ERROR_UNIT,*) ' PRINT ERROR STACK FROM: "'//__FILE__//'"', __LINE__
    CALL HOOKS%DEBUG_HOOK_%PRINT_ERROR_STACK( ERROR_UNIT )

    ! Free the error stack
    CALL HOOKS%DEBUG_HOOK_%FREE( )

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  RETURN

END FUNCTION MULTIO_GRIB2_RULES_OPEN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB2_RULES_CLOSE'
PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_RULES_CLOSE( C_RULES_PP ) &
 BIND(C,NAME='multio_grib2_rules_close') RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR

  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T
  USE :: RULES_WRAPPER_MOD,     ONLY: RULES_IDX_E
  USE :: RULES_WRAPPER_MOD,     ONLY: FREE_RULES
  USE :: API_F_C_WRAPPER_MOD,   ONLY: F_C_GET_INFO_WRAPPER
  USE :: API_GENERAL_UTILS_MOD, ONLY: DEREFERENCE_DOUBLE_C_POINTER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR), VALUE, INTENT(IN) :: C_RULES_PP

  !> Function result
  INTEGER(KIND=C_INT) :: RET

  !> Local variables
  TYPE(HOOKS_T) :: HOOKS
  TYPE(C_PTR), DIMENSION(:), POINTER :: C_RULES_P
  INTEGER(KIND=JPIB_K) :: OBJ_ID
  INTEGER(KIND=JPIB_K) :: OBJ_SIZE
  INTEGER(KIND=JPIB_K) :: BUF_SIZE
  INTEGER(KIND=JPIB_K) :: HASH

  !> Local error flags
  INTEGER(KIND=C_INT), PARAMETER :: ERRFLAG_ENCODER_NOT_ASSOCIATED = 1_JPIB_K
  INTEGER(KIND=C_INT), PARAMETER :: ERRFLAG_DEREFERENCE_DOUBLE_POINTER = 2_JPIB_K
  INTEGER(KIND=C_INT), PARAMETER :: ERRFLAG_UNABLE_TO_GET_INFO = 3_JPIB_K
  INTEGER(KIND=C_INT), PARAMETER :: ERRFLAG_UNABLE_FREE_ENCODER = 4_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )


  ! Initialization of the hooks
  CALL HOOKS%DEBUG_HOOK_%INIT( )

!$omp critical(API_ENCODER_MAP_REMOVE)

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(C_RULES_PP), ERRFLAG_ENCODER_NOT_ASSOCIATED )

  !> Dereference the double pointer
  C_RULES_P => NULL()
  PP_TRYCALL(ERRFLAG_DEREFERENCE_DOUBLE_POINTER) DEREFERENCE_DOUBLE_C_POINTER( &
&   C_RULES_PP, 1_JPIB_K, C_RULES_P, HOOKS )

  !> Extract info from the encoder
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_INFO) F_C_GET_INFO_WRAPPER( C_RULES_P(1), &
&   OBJ_ID, OBJ_SIZE, BUF_SIZE, HASH, HOOKS, VERIFY_CHECKSUM=.TRUE. )

  !> Free the encoder
  PP_TRYCALL(ERRFLAG_UNABLE_FREE_ENCODER) FREE_RULES( C_RULES_P(1), HOOKS )
  C_RULES_P(1) = C_NULL_PTR

!$omp end critical(API_ENCODER_MAP_REMOVE)

  !> Be sure we don't have any memory leaks
  CALL HOOKS%DEBUG_HOOK_%FREE( )

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

    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_ENCODER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoder not associated' )
    CASE (ERRFLAG_DEREFERENCE_DOUBLE_POINTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error dereferencing double pointer' )
    CASE (ERRFLAG_UNABLE_TO_GET_INFO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get info from the encoder' )
    CASE (ERRFLAG_UNABLE_FREE_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free the encoder' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown error' )
    END SELECT

    ! Print the error stack
    ! NOTE: This is importent when c is calling this function. Is opens the error_unit
    WRITE(ERROR_UNIT,*) ' PRINT ERROR STACK FROM: "'//__FILE__//'":', __LINE__
    CALL HOOKS%DEBUG_HOOK_%PRINT_ERROR_STACK( ERROR_UNIT )

    ! Free the error stack
    CALL HOOKS%DEBUG_HOOK_%FREE( )

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  RETURN

END FUNCTION MULTIO_GRIB2_RULES_CLOSE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB2_RULES_SEARCH'
PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_RULES_SEARCH( C_RULES_P, &
& C_MARS_DICT_P, C_TAG_PP, C_NAME_PP, C_SAMPLE_NAME_PP, C_RULE_PP ) &
 BIND(C,NAME='multio_grib2_rules_serch') RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER

  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIM_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_MOD,   ONLY: FORTRAN_MESSAGE_T
  USE :: API_SHARED_DATA_MOD,   ONLY: EXTRACT_MARS_DICTIONARY
  USE :: API_GENERAL_UTILS_MOD, ONLY: DEREFERENCE_DOUBLE_C_POINTER
  USE :: API_F_C_WRAPPER_MOD,   ONLY: F_C_GET_INFO_WRAPPER
  USE :: RULES_WRAPPER_MOD,     ONLY: SEARCH_RULE
  USE :: API_GENERAL_UTILS_MOD, ONLY: CONVERT_TO_C_STRING

  ! Symbols imported from other libraries
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: CPTR_FROM_YAML_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR), VALUE, INTENT(IN) :: C_RULES_P
  TYPE(C_PTR), VALUE, INTENT(IN) :: C_MARS_DICT_P
  TYPE(C_PTR), VALUE, INTENT(IN) :: C_TAG_PP
  TYPE(C_PTR), VALUE, INTENT(IN) :: C_NAME_PP
  TYPE(C_PTR), VALUE, INTENT(IN) :: C_SAMPLE_NAME_PP
  TYPE(C_PTR), VALUE, INTENT(IN) :: C_RULE_PP

  !> Function result
  INTEGER(KIND=C_INT) :: RET

  !> Local variables
  TYPE(HOOKS_T) :: HOOKS
  INTEGER(KIND=C_LONG_LONG), POINTER, DIMENSION(:) :: F_MARS_DICT_W
  TYPE(FORTRAN_MESSAGE_T), POINTER :: F_MARS_DICT
  TYPE(C_PTR), DIMENSION(:), POINTER :: C_TAG_P
  TYPE(C_PTR), DIMENSION(:), POINTER :: C_NAME_P
  TYPE(C_PTR), DIMENSION(:), POINTER :: C_SAMPLE_NAME_P
  TYPE(C_PTR), DIMENSION(:), POINTER :: C_RULE_P

  CHARACTER(LEN=256) :: F_TAG
  CHARACTER(LEN=256) :: F_NAME
  CHARACTER(LEN=256) :: F_SAMPLE_NAME
  TYPE(YAML_CONFIGURATION_T) :: F_RULE

  !> Local error flags
  INTEGER(KIND=C_INT), PARAMETER :: ERRFLAG_RULES_NOT_ASSOCIATED = 1_JPIB_K
  INTEGER(KIND=C_INT), PARAMETER :: ERRFLAG_MARS_NOT_ASSOCIATED = 2_JPIB_K
  INTEGER(KIND=C_INT), PARAMETER :: ERRFLAG_TAG_ALREADY_ASSOCIATED = 3_JPIB_K
  INTEGER(KIND=C_INT), PARAMETER :: ERRFLAG_NAME_ALREADY_ASSOCIATED = 4_JPIB_K
  INTEGER(KIND=C_INT), PARAMETER :: ERRFLAG_SAMPLE_NAME_ALREADY_ASSOCIATED = 5_JPIB_K
  INTEGER(KIND=C_INT), PARAMETER :: ERRFLAG_RULE_ALREADY_ASSOCIATED = 6_JPIB_K
  INTEGER(KIND=C_INT), PARAMETER :: ERRFLAG_EXTRACT_MARS_DICTIONARY = 7_JPIB_K
  INTEGER(KIND=C_INT), PARAMETER :: ERRFLAG_DEREFERENCE_DOUBLE_POINTER = 8_JPIB_K
  INTEGER(KIND=C_INT), PARAMETER :: ERRFLAG_SEARCH_RULE = 9_JPIB_K
  INTEGER(KIND=C_INT), PARAMETER :: ERRFLAG_CONVERT_TO_CSTRING = 10_JPIB_K
  INTEGER(KIND=C_INT), PARAMETER :: ERRFLAG_RULE_TO_CPTR = 11_JPIB_K

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

  ! Initialization of the hooks
  CALL HOOKS%DEBUG_HOOK_%INIT( )

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(C_RULES_P), ERRFLAG_RULES_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(C_MARS_DICT_P), ERRFLAG_MARS_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( C_ASSOCIATED(C_TAG_PP), ERRFLAG_TAG_ALREADY_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( C_ASSOCIATED(C_NAME_PP), ERRFLAG_NAME_ALREADY_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( C_ASSOCIATED(C_SAMPLE_NAME_PP), ERRFLAG_SAMPLE_NAME_ALREADY_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( C_ASSOCIATED(C_RULE_PP), ERRFLAG_RULE_ALREADY_ASSOCIATED )

  !> Get mars dictionary handle from the c pointer
  F_MARS_DICT_W => NULL()
  CALL C_F_POINTER( C_MARS_DICT_P, F_MARS_DICT_W, [2] )
  PP_DEBUG_CRITICAL_COND_THROW(.NOT.ASSOCIATED(F_MARS_DICT), ERRFLAG_EXTRACT_MARS_DICTIONARY )
  PP_TRYCALL(ERRFLAG_EXTRACT_MARS_DICTIONARY) EXTRACT_MARS_DICTIONARY( F_MARS_DICT_W, F_MARS_DICT, HOOKS )

  !> Dereference the double pointers
  C_TAG_P => NULL()
  PP_TRYCALL(ERRFLAG_DEREFERENCE_DOUBLE_POINTER) DEREFERENCE_DOUBLE_C_POINTER( &
&   C_TAG_PP, 1_JPIB_K, C_TAG_P, HOOKS )
  C_NAME_P => NULL()
  PP_TRYCALL(ERRFLAG_DEREFERENCE_DOUBLE_POINTER) DEREFERENCE_DOUBLE_C_POINTER( &
&   C_NAME_PP, 1_JPIB_K, C_NAME_P, HOOKS )
  C_SAMPLE_NAME_P => NULL()
  PP_TRYCALL(ERRFLAG_DEREFERENCE_DOUBLE_POINTER) DEREFERENCE_DOUBLE_C_POINTER( &
&   C_SAMPLE_NAME_PP, 1_JPIB_K, C_SAMPLE_NAME_P, HOOKS )
  C_RULE_P => NULL()
  PP_TRYCALL(ERRFLAG_DEREFERENCE_DOUBLE_POINTER) DEREFERENCE_DOUBLE_C_POINTER( &
&   C_RULE_PP, 1_JPIB_K, C_RULE_P, HOOKS )

  !> Get the rules object info
  PP_TRYCALL(ERRFLAG_SEARCH_RULE) SEARCH_RULE( C_RULES_P, &
&    F_MARS_DICT, F_TAG, F_NAME, F_SAMPLE_NAME, F_RULE, &
&    HOOKS )

  !> Populate output pointers
  PP_TRYCALL(ERRFLAG_CONVERT_TO_CSTRING) CONVERT_TO_C_STRING( &
    TRIM(ADJUSTL(F_TAG)), C_TAG_P(1), HOOKS )
  PP_TRYCALL(ERRFLAG_CONVERT_TO_CSTRING) CONVERT_TO_C_STRING( &
    TRIM(ADJUSTL(F_NAME)), C_NAME_P(1), HOOKS )
  PP_TRYCALL(ERRFLAG_CONVERT_TO_CSTRING) CONVERT_TO_C_STRING( &
    TRIM(ADJUSTL(F_SAMPLE_NAME)), C_SAMPLE_NAME_P(1), HOOKS )

  !> Extact cpointr of yaml configuration
  PP_TRYCALL(ERRFLAG_RULE_TO_CPTR) CPTR_FROM_YAML_CONFIGURATION( &
&    F_RULE, C_RULE_P(1), HOOKS )

  !> Be sure we don't have any memory leaks
  CALL HOOKS%DEBUG_HOOK_%FREE( )

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

    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_RULES_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Rules not associated' )
    CASE (ERRFLAG_MARS_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'MARS dictionary not associated' )
    CASE (ERRFLAG_TAG_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Tag already associated' )
    CASE (ERRFLAG_NAME_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Name already associated' )
    CASE (ERRFLAG_SAMPLE_NAME_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Sample name already associated' )
    CASE (ERRFLAG_RULE_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Rule already associated' )
    CASE (ERRFLAG_EXTRACT_MARS_DICTIONARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract MARS dictionary' )
    CASE (ERRFLAG_DEREFERENCE_DOUBLE_POINTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error dereferencing double pointer' )
    CASE (ERRFLAG_SEARCH_RULE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to search rule' )
    CASE (ERRFLAG_CONVERT_TO_CSTRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert string to C string' )
    CASE (ERRFLAG_RULE_TO_CPTR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert rule to C pointer' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown error' )
    END SELECT

    ! Print the error stack
    ! NOTE: This is important when c is calling this function. Is opens the error_unit
    WRITE(ERROR_UNIT,*) ' PRINT ERROR STACK FROM: "'//__FILE__//'":', __LINE__
    CALL HOOKS%DEBUG_HOOK_%PRINT_ERROR_STACK( ERROR_UNIT )

    ! Free the error stack
    CALL HOOKS%DEBUG_HOOK_%FREE( )

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  RETURN

END FUNCTION MULTIO_GRIB2_RULES_SEARCH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB2_RULES_PRINT'
PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_RULES_PRINT( C_ENCODER_MTG2, &
& C_MARS_DICT, C_PAR_DICT, C_GRIB_SAMPLE_ID_P ) &
 BIND(C,NAME='multio_grib2_rules_print_f') RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER

  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIM_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR),  VALUE, INTENT(IN) :: C_ENCODER_MTG2
  TYPE(C_PTR),  VALUE, INTENT(IN) :: C_MARS_DICT
  TYPE(C_PTR),  VALUE, INTENT(IN) :: C_PAR_DICT
  TYPE(C_PTR),  VALUE, INTENT(IN) :: C_GRIB_SAMPLE_ID_P

  !> Function result
  INTEGER(KIND=C_INT) :: RET

  !> Local variables
  TYPE(HOOKS_T) :: HOOKS

  !> Local error flags

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

  ! Initialization of the hooks
  CALL HOOKS%DEBUG_HOOK_%INIT( )



  !> Be sure we don't have any memory leaks
  CALL HOOKS%DEBUG_HOOK_%FREE( )

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

    CHARACTER(LEN=32) :: CTMP

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    SELECT CASE(ERRIDX)
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown error' )
    END SELECT

    ! Print the error stack
    ! NOTE: This is important when c is calling this function. Is opens the error_unit
    WRITE(ERROR_UNIT,*) ' PRINT ERROR STACK FROM: "'//__FILE__//'":', __LINE__
    CALL HOOKS%DEBUG_HOOK_%PRINT_ERROR_STACK( ERROR_UNIT )

    ! Free the error stack
    CALL HOOKS%DEBUG_HOOK_%FREE( )

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  RETURN

END FUNCTION MULTIO_GRIB2_RULES_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE RULES_API_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
