! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'api_dictionary_wrapper_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'API_DICTIONARY_WRAPPER_MOD'
MODULE API_DICTIONARY_WRAPPER_MOD

IMPLICIT NONE

!> Default visibility of the module
PRIVATE


! Whitelist of public symbols (dictionaries management)
PUBLIC :: MULTIO_GRIB2_INIT_OPTIONS
PUBLIC :: MULTIO_GRIB2_DICT_CREATE
PUBLIC :: MULTIO_GRIB2_DICT_DESTROY
PUBLIC :: MULTIO_GRIB2_DICT_SET
PUBLIC :: MULTIO_GRIB2_DICT_GET
PUBLIC :: MULTIO_GRIB2_DICT_HAS
PUBLIC :: MULTIO_GRIB2_DICT_ITERATE

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB2_INIT_OPTIONS'
PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_INIT_OPTIONS( DICT ) &
 BIND(C,NAME='multio_grib2_init_options') RESULT(RET)

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
  TYPE(C_PTR), INTENT(INOUT) :: DICT

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

END FUNCTION MULTIO_GRIB2_INIT_OPTIONS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB2_DICT_CREATE'
PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_DICT_CREATE( MULTIO_GRIB2, DICT_TYPE, LEN ) &
 BIND(C,NAME='multio_grib2_dict_create_f') RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,       ONLY: JPIB_K
  USE :: HOOKS_MOD,               ONLY: HOOKS_T
  USE :: MAP_INT64_MARS_DICT_MOD, ONLY: MAP_FUNCTION_INT64_MARS_DICT_IF
  USE :: MAP_INT64_PAR_DICT_MOD,  ONLY: MAP_FUNCTION_INT64_PAR_DICT_IF
  USE :: API_SHARED_DATA_MOD,     ONLY: SHARED_MARS_DICT_MAP
  USE :: API_SHARED_DATA_MOD,     ONLY: SHARED_PAR_DICT_MAP
  USE :: API_SHARED_DATA_MOD,     ONLY: FREE_MARS_MESSAGE
  USE :: API_SHARED_DATA_MOD,     ONLY: FREE_PARAMETRIZATION
  USE :: FORTRAN_MESSAGE_MOD,     ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,     ONLY: PARAMETRIZATION_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR),                INTENT(INOUT) :: MULTIO_GRIB2
  TYPE(C_PTR), VALUE,         INTENT(IN)    :: DICT_TYPE
  INTEGER(KIND=C_INT), VALUE, INTENT(IN)    :: LEN

  !> Function result
  INTEGER(KIND=C_INT) :: RET

  !> Local variables
  LOGICAL :: INITIALIZED
  INTEGER(KIND=C_LONG_LONG), POINTER, DIMENSION(:) :: F_MULTIO_GRIB2
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: SIZE
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  INTEGER(KIND=C_LONG_LONG), POINTER, DIMENSION(:) :: MAX_DICT_HANDLE
  TYPE(HOOKS_T) :: HOOKS
  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(:), POINTER :: DICT_TYPE_C
  CHARACTER(LEN=32) :: DICT_TYPE_NAME

  TYPE(FORTRAN_MESSAGE_T), POINTER :: MESSAGE
  PROCEDURE(MAP_FUNCTION_INT64_MARS_DICT_IF), POINTER :: MESSAGE_DESTRUCTOR
  TYPE(PARAMETRIZATION_T), POINTER :: PARAMETRIZATION
  PROCEDURE(MAP_FUNCTION_INT64_PAR_DICT_IF), POINTER :: PARAMETRIZATION_DESTRUCTOR

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOC_FAILURE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ADD_DICTIONARY=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INIT_MAP=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_SIZE=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_MAXIMUM=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_LENGTH=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DICTIONARY_ALREADY_ASSOCIATED=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DICTYPE_NOT_ASSOCIATED=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TOCHECK_INITIALIZATION_STATUS=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_DICTIONARY_TYPE=11_JPIB_K

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

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( C_ASSOCIATED(MULTIO_GRIB2),   ERRFLAG_DICTIONARY_ALREADY_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(DICT_TYPE), ERRFLAG_DICTYPE_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( LEN .LE. 0, ERRFLAG_INVALID_LENGTH )

  !> Get the size of the dictionary type
  CALL C_F_POINTER( DICT_TYPE, DICT_TYPE_C, [LEN] )

  ! Copy the dictionary type to a fortran string
  DICT_TYPE_NAME = REPEAT(' ', 32)
  DO I = 1, LEN
    DICT_TYPE_NAME(I:I) = DICT_TYPE_C(I)
  END DO

  !TODO: Convert to lowercase

  SELECT CASE( TRIM(DICT_TYPE_NAME) )

  CASE ('mars', 'm', 'mars-dict', 'mars_dict')

    !> Get the dictionary handle from the c pointer
    ALLOCATE( F_MULTIO_GRIB2(2), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_ALLOC_FAILURE )

    !> Check if the dictionary is already in the dictionarys map
    PP_TRYCALL(ERRFLAG_UNABLE_TOCHECK_INITIALIZATION_STATUS) SHARED_MARS_DICT_MAP%INITIALIZED( INITIALIZED, HOOKS )

    !> Conditionally initialized the dictionarys map
!$omp single
    IF ( .NOT. INITIALIZED ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_INIT_MAP) SHARED_MARS_DICT_MAP%INIT( HOOKS )
    ENDIF
!$omp end single

!$omp critical(API_DICTIONARY_MAP_INSERT)
    !> Get the size of the dictionary map
    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_SIZE) SHARED_MARS_DICT_MAP%SIZE( SIZE, HOOKS )

    IF ( SIZE .EQ. 0_JPIB_K ) THEN
      F_MULTIO_GRIB2(1) = 10_C_LONG_LONG
      F_MULTIO_GRIB2(2) = 1_C_LONG_LONG
    ELSE
      !> Check if the dictionary is already in the dictionarys map
      PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_MAXIMUM) SHARED_MARS_DICT_MAP%MAX( MAX_DICT_HANDLE, HOOKS )

      !> Increment the dictionary handle
      F_MULTIO_GRIB2(1) = 10_C_LONG_LONG
      F_MULTIO_GRIB2(2) = MAX_DICT_HANDLE(2) + 1_C_LONG_LONG
    ENDIF

    !> Allocate the mars dictionary
    ALLOCATE( MESSAGE, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_ALLOC_FAILURE )

    MESSAGE_DESTRUCTOR => FREE_MARS_MESSAGE
    PP_TRYCALL(ERRFLAG_UNABLE_TO_ADD_DICTIONARY) SHARED_MARS_DICT_MAP%INSERT( F_MULTIO_GRIB2, MESSAGE, MESSAGE_DESTRUCTOR, HOOKS )
!$omp end critical(API_DICTIONARY_MAP_INSERT)

    !> Get the location of the dictionary
    MULTIO_GRIB2 = C_LOC( F_MULTIO_GRIB2 )

  CASE ('parametrization', 'p', 'par', 'param', 'parametrization-dict', 'parametrization_dict', 'par-dict', 'par_dict', 'param-dict', 'param_dict')


    !> Get the dictionary handle from the c pointer
    ALLOCATE( F_MULTIO_GRIB2(2), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_ALLOC_FAILURE )

    !> Check if the dictionary is already in the dictionarys map
    PP_TRYCALL(ERRFLAG_UNABLE_TOCHECK_INITIALIZATION_STATUS) SHARED_PAR_DICT_MAP%INITIALIZED( INITIALIZED, HOOKS )

    !> Conditionally initialized the dictionarys map
!$omp single
    IF ( .NOT. INITIALIZED ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_INIT_MAP) SHARED_PAR_DICT_MAP%INIT( HOOKS )
    ENDIF
!$omp end single

!$omp critical(API_DICTIONARY_MAP_INSERT)
    !> Get the size of the dictionary map
    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_SIZE) SHARED_PAR_DICT_MAP%SIZE( SIZE, HOOKS )

    IF ( SIZE .EQ. 0_JPIB_K ) THEN
      F_MULTIO_GRIB2(1) = 20_C_LONG_LONG
      F_MULTIO_GRIB2(2) = 1_C_LONG_LONG
    ELSE
      !> Check if the dictionary is already in the dictionarys map
      PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_MAXIMUM) SHARED_PAR_DICT_MAP%MAX( MAX_DICT_HANDLE, HOOKS )

      !> Increment the dictionary handle
      F_MULTIO_GRIB2(1) = 20_C_LONG_LONG
      F_MULTIO_GRIB2(2) = MAX_DICT_HANDLE(2) + 1_C_LONG_LONG
    ENDIF

    !> Allocate the mars dictionary
    ALLOCATE( PARAMETRIZATION, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_ALLOC_FAILURE )

    PARAMETRIZATION_DESTRUCTOR => FREE_PARAMETRIZATION
    PP_TRYCALL(ERRFLAG_UNABLE_TO_ADD_DICTIONARY) SHARED_PAR_DICT_MAP%INSERT( F_MULTIO_GRIB2, PARAMETRIZATION, PARAMETRIZATION_DESTRUCTOR, HOOKS )
!$omp end critical(API_DICTIONARY_MAP_INSERT)

    !> Get the location of the dictionary
    MULTIO_GRIB2 = C_LOC( F_MULTIO_GRIB2 )

  ! CASE ('options_dict')

  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_DICTIONARY_TYPE )
  END SELECT


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
    CASE (ERRFLAG_DICTIONARY_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Dictionary already associated' )
    CASE (ERRFLAG_DICTYPE_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Dictionary type not associated' )
    CASE (ERRFLAG_INVALID_LENGTH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid length for "dic_type"' )
    CASE (ERRFLAG_ALLOC_FAILURE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Allocation failure' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: ' // TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG, STAT=ALLOC_STATUS)
      END IF
    CASE (ERRFLAG_UNABLE_TO_INIT_MAP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to initialize the map' )
    CASE (ERRFLAG_UNABLE_TO_GET_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the size of the map' )
    CASE (ERRFLAG_UNABLE_TO_GET_MAXIMUM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the maximum of the map' )
    CASE (ERRFLAG_UNABLE_TO_ADD_DICTIONARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to add the dictionary to the map' )
    CASE (ERRFLAG_UNABLE_TOCHECK_INITIALIZATION_STATUS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check the initialization status of the map' )
    CASE (ERRFLAG_UNKNOWN_DICTIONARY_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown dictionary type' )
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

END FUNCTION MULTIO_GRIB2_DICT_CREATE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE






#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB2_DICT_DESTROY'
PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_DICT_DESTROY( MULTIO_GRIB2 ) &
 BIND(C,NAME='multio_grib2_dict_destroy') RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: API_SHARED_DATA_MOD, ONLY: SHARED_MARS_DICT_MAP
  USE :: API_SHARED_DATA_MOD, ONLY: SHARED_PAR_DICT_MAP

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

  !> Local variables
  LOGICAL :: INITIALIZED
  LOGICAL :: MAP_HAS_DICTIONARY
  LOGICAL :: DICTIONARY_REMOVED
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  INTEGER(KIND=JPIB_K) :: MAP_SIZE
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  INTEGER(KIND=C_LONG_LONG), POINTER, DIMENSION(:) :: F_MULTIO_GRIB2
  TYPE(HOOKS_T) :: HOOKS

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DICTIONARY_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAP_NOT_INITIALIZED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCH_DICTIONARY=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_REMOVE_DICTIONARY=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAP_LIST=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_NOT_ASSOCIATED=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOC_FAILURE=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_MAP=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_SIZE=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_HANDLE=10_JPIB_K

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

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(MULTIO_GRIB2), ERRFLAG_DICTIONARY_NOT_ASSOCIATED )

  !> Get th fortran handle from the c handle
  F_MULTIO_GRIB2 => NULL()
  CALL C_F_POINTER( MULTIO_GRIB2, F_MULTIO_GRIB2, [2] )

  !> Check the allocation status of the fortran handle
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(F_MULTIO_GRIB2), ERRFLAG_KEY_NOT_ASSOCIATED )

  !> Depending on the dictionary type we have to deallocate the dictionary
  SELECT CASE ( F_MULTIO_GRIB2(1) )

  CASE ( 10_C_LONG_LONG )

!$omp critical(API_DICTIONARY_MAP_REMOVE)

    !> Print the mars dictionary map
    !> NOTE: To be uncommented for debugging the API
    ! PP_TRYCALL(ERRFLAG_MAP_LIST) SHARED_MARS_DICT_MAP%LIST( 6_JPIB_K, 'MARS_DICTIONARY_MAP: ', HOOKS )

    !> Check if the dictionary is already in the dictionarys map
    PP_TRYCALL(ERRFLAG_MAP_NOT_INITIALIZED) SHARED_MARS_DICT_MAP%INITIALIZED( INITIALIZED, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT. INITIALIZED, ERRFLAG_MAP_NOT_INITIALIZED )

    !> Check if the handle is associated to an dictionary
    PP_TRYCALL(ERRFLAG_MATCH_DICTIONARY) SHARED_MARS_DICT_MAP%HAS( F_MULTIO_GRIB2, MAP_HAS_DICTIONARY, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT. MAP_HAS_DICTIONARY, ERRFLAG_MATCH_DICTIONARY )

    !> Remove the dictionary from the map
    PP_TRYCALL(ERRFLAG_REMOVE_DICTIONARY) SHARED_MARS_DICT_MAP%REMOVE( F_MULTIO_GRIB2, DICTIONARY_REMOVED, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT. DICTIONARY_REMOVED, ERRFLAG_REMOVE_DICTIONARY )

    !> Print the dictionary map
    !> NOTE: To be uncommented for debugging the API
    ! PP_TRYCALL(ERRFLAG_MAP_LIST) SHARED_ESHARED_MARS_DICT_MAPNCODER_MAP%LIST( 6_JPIB_K, 'MARS_DICTIONARY_MAP: ', HOOKS )

    !> Get the map size
    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_SIZE) SHARED_MARS_DICT_MAP%SIZE( MAP_SIZE, HOOKS )

    !> To avoid the need of init/exit API, every time the map is empty we deallocate the map
    IF ( MAP_SIZE .EQ. 0_JPIB_K ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_MAP) SHARED_MARS_DICT_MAP%FREE( HOOKS )
    ENDIF
!$omp end critical(API_DICTIONARY_MAP_REMOVE)

  CASE ( 20_C_LONG_LONG )


!$omp critical(API_DICTIONARY_MAP_REMOVE)

    !> Print the parametrization dictionary map
    !> NOTE: To be uncommented for debugging the API
    ! PP_TRYCALL(ERRFLAG_MAP_LIST) SHARED_PAR_DICT_MAP%LIST( 6_JPIB_K, 'PARAMETRIZATION_DICTIONARY_MAP: ', HOOKS )

    !> Check if the dictionary is already in the dictionarys map
    PP_TRYCALL(ERRFLAG_MAP_NOT_INITIALIZED) SHARED_PAR_DICT_MAP%INITIALIZED( INITIALIZED, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT. INITIALIZED, ERRFLAG_MAP_NOT_INITIALIZED )

    !> Check if the handle is associated to an dictionary
    PP_TRYCALL(ERRFLAG_MATCH_DICTIONARY) SHARED_PAR_DICT_MAP%HAS( F_MULTIO_GRIB2, MAP_HAS_DICTIONARY, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT. MAP_HAS_DICTIONARY, ERRFLAG_MATCH_DICTIONARY )

    !> Remove the dictionary from the map
    PP_TRYCALL(ERRFLAG_REMOVE_DICTIONARY) SHARED_PAR_DICT_MAP%REMOVE( F_MULTIO_GRIB2, DICTIONARY_REMOVED, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT. DICTIONARY_REMOVED, ERRFLAG_REMOVE_DICTIONARY )

    !> Print the dictionary map
    !> NOTE: To be uncommented for debugging the API
    ! PP_TRYCALL(ERRFLAG_MAP_LIST) SHARED_PAR_DICT_MAP%LIST( 6_JPIB_K, 'PARAMETRIZATION_DICTIONARY_MAP: ', HOOKS )

    !> Get the map size
    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_SIZE) SHARED_PAR_DICT_MAP%SIZE( MAP_SIZE, HOOKS )

    !> To avoid the need of init/exit API, every time the map is empty we deallocate the map
    IF ( MAP_SIZE .EQ. 0_JPIB_K ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_MAP) SHARED_PAR_DICT_MAP%FREE( HOOKS )
    ENDIF
!$omp end critical(API_DICTIONARY_MAP_REMOVE)

  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_WRONG_HANDLE )
  END SELECT

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
    CASE (ERRFLAG_DICTIONARY_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Dictionary not associated' )
    CASE (ERRFLAG_KEY_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key not associated' )
    CASE (ERRFLAG_MAP_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Map not initialized' )
    CASE (ERRFLAG_MATCH_DICTIONARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to match the dictionary' )
    CASE (ERRFLAG_REMOVE_DICTIONARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to remove the dictionary' )
    CASE (ERRFLAG_MAP_LIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to list the map' )
    CASE (ERRFLAG_DEALLOC_FAILURE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key deallocation failure' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: ' // TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG, STAT=DEALLOC_STAT)
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_FREE_MAP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free the map' )
    CASE (ERRFLAG_UNABLE_TO_GET_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the size of the map' )
    CASE (ERRFLAG_WRONG_HANDLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong handle (handle is not from an dictionary)' )
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

END FUNCTION MULTIO_GRIB2_DICT_DESTROY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB2_DICT_SET'
PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_DICT_SET( DICT, KEY, KLEN, VALUE, VLEN ) &
 BIND(C,NAME='multio_grib2_dict_set_f') RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: API_SHARED_DATA_MOD, ONLY: EXTRACT_MARS_DICTIONARY
  USE :: API_SHARED_DATA_MOD, ONLY: EXTRACT_PAR_DICTIONARY
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR),         VALUE, INTENT(IN) :: DICT
  TYPE(C_PTR),         VALUE, INTENT(IN) :: KEY
  INTEGER(KIND=C_INT), VALUE, INTENT(IN) :: KLEN
  TYPE(C_PTR),         VALUE, INTENT(IN) :: VALUE
  INTEGER(KIND=C_INT), VALUE, INTENT(IN) :: VLEN

  !> Function result
  INTEGER(KIND=C_INT) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=C_LONG_LONG), POINTER, DIMENSION(:) :: F_DICT
  TYPE(FORTRAN_MESSAGE_T), POINTER :: MARS_DICT
  TYPE(PARAMETRIZATION_T), POINTER :: PAR_DICT
  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(:), POINTER :: C_TMP_KEY
  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(:), POINTER :: C_TMP_VAL
  CHARACTER(LEN=KLEN) :: F_KEY
  CHARACTER(LEN=VLEN) :: F_VAL
  TYPE(HOOKS_T) :: HOOKS

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DICTIONARY_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_NOT_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUE_NOT_ASSOCIATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_HANDLE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_EXTRACT_MARS_DICTIONARY=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_EXTRACT_PAR_DICTIONARY=6_JPIB_K

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

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(DICT), ERRFLAG_DICTIONARY_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(KEY),  ERRFLAG_KEY_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(VALUE), ERRFLAG_VALUE_NOT_ASSOCIATED )

  !> Get the size of the dictionary type
  CALL C_F_POINTER( KEY,   C_TMP_KEY, [KLEN] )
  CALL C_F_POINTER( VALUE, C_TMP_VAL, [VLEN] )

  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(C_TMP_KEY), ERRFLAG_KEY_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(C_TMP_VAL), ERRFLAG_VALUE_NOT_ASSOCIATED )

  ! Copy the key to a fortran string
  F_KEY = REPEAT(' ', KLEN)
  DO I = 1, KLEN
    F_KEY(I:I) = C_TMP_KEY(I)
  ENDDO
  C_TMP_KEY => NULL()

  ! Copy the key to a fortran string
  F_VAL = REPEAT(' ', VLEN)
  DO I = 1, VLEN
    F_VAL(I:I) = C_TMP_VAL(I)
  ENDDO
  C_TMP_VAL => NULL()


  !> Get th fortran handle from the c handle
  F_DICT => NULL()
  CALL C_F_POINTER( DICT, F_DICT, [2] )

  !> Check the allocation status of the fortran handle
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(F_DICT), ERRFLAG_KEY_NOT_ASSOCIATED )

  !> Depending on the dictionary type we have to deallocate the dictionary
  SELECT CASE ( F_DICT(1) )

  CASE ( 10_C_LONG_LONG )

    PP_TRYCALL(ERRFLAG_EXTRACT_MARS_DICTIONARY) EXTRACT_MARS_DICTIONARY( F_DICT, MARS_DICT, HOOKS )

    ! TODO: Set the value
    WRITE(*,*) 'KEY: ', F_KEY
    WRITE(*,*) 'VAL: ', F_VAL

  CASE ( 20_C_LONG_LONG )

    PP_TRYCALL(ERRFLAG_EXTRACT_PAR_DICTIONARY) EXTRACT_PAR_DICTIONARY( F_DICT, PAR_DICT, HOOKS )

    ! TODO: Set the value

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_WRONG_HANDLE )

  END SELECT

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
    CASE (ERRFLAG_DICTIONARY_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Dictionary not associated' )
    CASE (ERRFLAG_KEY_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key not associated' )
    CASE (ERRFLAG_VALUE_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value not associated' )
    CASE (ERRFLAG_WRONG_HANDLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong handle (handle is not from an dictionary)' )
    CASE (ERRFLAG_EXTRACT_MARS_DICTIONARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract the mars dictionary' )
    CASE (ERRFLAG_EXTRACT_PAR_DICTIONARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract the parametrization dictionary' )
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

END FUNCTION MULTIO_GRIB2_DICT_SET
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB2_DICT_GET'
PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_DICT_GET( DICT, KEY, KLEN, VALUE ) &
 BIND(C,NAME='multio_grib2_dict_get_f') RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: API_SHARED_DATA_MOD, ONLY: EXTRACT_MARS_DICTIONARY
  USE :: API_SHARED_DATA_MOD, ONLY: EXTRACT_PAR_DICTIONARY
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR), VALUE,         INTENT(IN)    :: DICT
  TYPE(C_PTR), VALUE,         INTENT(IN)    :: KEY
  INTEGER(KIND=C_INT), VALUE, INTENT(IN)    :: KLEN
  TYPE(C_PTR),                INTENT(INOUT) :: VALUE

  !> Function result
  INTEGER(KIND=C_INT) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=C_LONG_LONG), POINTER, DIMENSION(:) :: F_DICT
  TYPE(FORTRAN_MESSAGE_T), POINTER :: MARS_DICT
  TYPE(PARAMETRIZATION_T), POINTER :: PAR_DICT
  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(:), POINTER :: C_TMP_KEY
  CHARACTER(LEN=KLEN) :: F_KEY
  TYPE(HOOKS_T) :: HOOKS

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DICTIONARY_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_NOT_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_HANDLE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_EXTRACT_MARS_DICTIONARY=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_EXTRACT_PAR_DICTIONARY=6_JPIB_K

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

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(DICT), ERRFLAG_DICTIONARY_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(KEY),  ERRFLAG_KEY_NOT_ASSOCIATED )

  !> Get the size of the dictionary type
  CALL C_F_POINTER( KEY,   C_TMP_KEY, [KLEN] )

  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(C_TMP_KEY), ERRFLAG_KEY_NOT_ASSOCIATED )

  ! Copy the key to a fortran string
  F_KEY = REPEAT(' ', KLEN)
  DO I = 1, KLEN
    F_KEY(I:I) = C_TMP_KEY(I)
  ENDDO
  C_TMP_KEY => NULL()

  !> Get th fortran handle from the c handle
  F_DICT => NULL()
  CALL C_F_POINTER( DICT, F_DICT, [2] )

  !> Check the allocation status of the fortran handle
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(F_DICT), ERRFLAG_KEY_NOT_ASSOCIATED )

  !> Depending on the dictionary type we have to deallocate the dictionary
  SELECT CASE ( F_DICT(1) )

  CASE ( 10_C_LONG_LONG )

    PP_TRYCALL(ERRFLAG_EXTRACT_MARS_DICTIONARY) EXTRACT_MARS_DICTIONARY( F_DICT, MARS_DICT, HOOKS )

    ! TODO: Utility to allocate a string that can be deallocated from c
    WRITE(*,*) 'KEY: ', F_KEY


  CASE ( 20_C_LONG_LONG )

    PP_TRYCALL(ERRFLAG_EXTRACT_PAR_DICTIONARY) EXTRACT_PAR_DICTIONARY( F_DICT, PAR_DICT, HOOKS )

    ! TODO: Set the value

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_WRONG_HANDLE )

  END SELECT

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
    CASE (ERRFLAG_DICTIONARY_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Dictionary not associated' )
    CASE (ERRFLAG_KEY_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key not associated' )
    CASE (ERRFLAG_WRONG_HANDLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong handle (handle is not from an dictionary)' )
    CASE (ERRFLAG_EXTRACT_MARS_DICTIONARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract the mars dictionary' )
    CASE (ERRFLAG_EXTRACT_PAR_DICTIONARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract the parametrization dictionary' )
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

END FUNCTION MULTIO_GRIB2_DICT_GET
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB2_DICT_HAS'
PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_DICT_HAS( DICT, KEY, KLEN, HAS ) &
 BIND(C,NAME='multio_grib2_dict_has_f') RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: API_SHARED_DATA_MOD, ONLY: EXTRACT_MARS_DICTIONARY
  USE :: API_SHARED_DATA_MOD, ONLY: EXTRACT_PAR_DICTIONARY
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR), VALUE,         INTENT(IN)    :: DICT
  TYPE(C_PTR), VALUE,         INTENT(IN)    :: KEY
  INTEGER(KIND=C_INT), VALUE, INTENT(IN)    :: KLEN
  INTEGER(KIND=C_INT),        INTENT(INOUT) :: HAS

  !> Function result
  INTEGER(KIND=C_INT) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=C_LONG_LONG), POINTER, DIMENSION(:) :: F_DICT
  TYPE(FORTRAN_MESSAGE_T), POINTER :: MARS_DICT
  TYPE(PARAMETRIZATION_T), POINTER :: PAR_DICT
  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(:), POINTER :: C_TMP_KEY
  CHARACTER(LEN=KLEN) :: F_KEY
  TYPE(HOOKS_T) :: HOOKS

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DICTIONARY_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_NOT_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_HANDLE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_EXTRACT_MARS_DICTIONARY=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_EXTRACT_PAR_DICTIONARY=6_JPIB_K

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

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(DICT), ERRFLAG_DICTIONARY_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(KEY),  ERRFLAG_KEY_NOT_ASSOCIATED )

  !> Get the size of the dictionary type
  CALL C_F_POINTER( KEY, C_TMP_KEY, [KLEN] )

  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(C_TMP_KEY), ERRFLAG_KEY_NOT_ASSOCIATED )

  ! Copy the key to a fortran string
  F_KEY = REPEAT(' ', KLEN)
  DO I = 1, KLEN
    F_KEY(I:I) = C_TMP_KEY(I)
  ENDDO
  C_TMP_KEY => NULL()

  !> Get th fortran handle from the c handle
  F_DICT => NULL()
  CALL C_F_POINTER( DICT, F_DICT, [2] )

  !> Check the allocation status of the fortran handle
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(F_DICT), ERRFLAG_KEY_NOT_ASSOCIATED )

  !> Depending on the dictionary type we have to deallocate the dictionary
  SELECT CASE ( F_DICT(1) )

  CASE ( 10_C_LONG_LONG )

    PP_TRYCALL(ERRFLAG_EXTRACT_MARS_DICTIONARY) EXTRACT_MARS_DICTIONARY( F_DICT, MARS_DICT, HOOKS )

    ! TODO: Utility to allocate a string that can be deallocated from c
    WRITE(*,*) 'KEY: ', F_KEY
    HAS = 0_C_INT


  CASE ( 20_C_LONG_LONG )

    PP_TRYCALL(ERRFLAG_EXTRACT_PAR_DICTIONARY) EXTRACT_PAR_DICTIONARY( F_DICT, PAR_DICT, HOOKS )

    ! TODO: Set the value

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_WRONG_HANDLE )

  END SELECT

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
    CASE (ERRFLAG_DICTIONARY_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Dictionary not associated' )
    CASE (ERRFLAG_KEY_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key not associated' )
    CASE (ERRFLAG_WRONG_HANDLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong handle (handle is not from an dictionary)' )
    CASE (ERRFLAG_EXTRACT_MARS_DICTIONARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract the mars dictionary' )
    CASE (ERRFLAG_EXTRACT_PAR_DICTIONARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract the parametrization dictionary' )
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

END FUNCTION MULTIO_GRIB2_DICT_HAS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB2_DICT_ITERATE'
PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_DICT_ITERATE( DICT, IT, KEY, HAS ) &
 BIND(C,NAME='multio_grib2_dict_iterate') RESULT(RET)

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
  TYPE(C_PTR), VALUE, INTENT(IN)    :: DICT
  TYPE(C_PTR),        INTENT(INOUT) :: IT
  TYPE(C_PTR),        INTENT(INOUT) :: KEY
  TYPE(C_PTR),        INTENT(INOUT) :: HAS

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

END FUNCTION MULTIO_GRIB2_DICT_ITERATE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE API_DICTIONARY_WRAPPER_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME