!>
!> @file filter_level_mod.F90
!>
!> @brief Module containing definitions and procedures for level filters.
!>
!> This module defines the `FILTER_LEVELIST_T` type, along with its associated
!> procedures and helper functions that facilitate the creation, management, and
!> utilization of level filters within the system. Level filters allow for
!> complex filtering operations by combining multiple nested filters.
!>
!> @author Mirco Valentini
!> @date   August, 2024
!>

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'multiom_encoder_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'MULTIOM_CACHED_ENCODER_MOD'
MODULE MULTIOM_CACHED_ENCODER_MOD

  ! Symbols imported from other modules within the project.
  USE :: MAPPING_RULES_COLLECTION_MOD, ONLY: MAPPING_RULES_COLLECTION_T
  USE :: ENCODING_RULE_COLLECTION_MOD, ONLY: ENCODING_RULE_COLLECTION_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: MAPPING_CACHE_MOD,        ONLY: MAPPING_CACHE_T
  USE :: ENCODING_CACHE_MOD,       ONLY: ENCODING_CACHE_T
  USE :: FILTER_OPTIONS_MOD,       ONLY: FILTER_OPTIONS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: MAPPING_OPTIONS_MOD,      ONLY: MAPPING_OPTIONS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!> Type used to store a grib encoder
TYPE :: MULTIOM_CACHED_ENCODERS_T

  !> Default visibility of the type
  PRIVATE

  !> Metadata used for starting the encoding of each field
  CLASS(METADATA_BASE_A), POINTER :: METADATA => NULL()

  !> Rules used for encoding
  TYPE(MAPPING_RULES_COLLECTION_T) :: MAPPING_RULES
  TYPE(ENCODING_RULE_COLLECTION_T) :: ENCODER_RULES

  !> Cache used for encoding
  TYPE(ENCODING_CACHE_T) :: ENCODER_CACHE
  TYPE(MAPPING_CACHE_T)  :: MAPPING_CACHE

  !> Options
  TYPE(CACHE_OPTIONS_T)        :: CACHE_OPTIONS
  TYPE(GRIB_ENCODER_OPTIONS_T) :: ENCODER_OPTIONS
  TYPE(MAPPING_OPTIONS_T)      :: MAPPING_OPTIONS
  TYPE(FILTER_OPTIONS_T)       :: FILTER_OPTIONS

CONTAINS

  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: INIT   => MULTIO_ENCODER_INIT
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: ENCODE => MULTIO_ENCODER_ENCODE
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: PRINT  => MULTIO_ENCODER_PRINT
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: ENCODING_CACHE_DUMP => MULTIO_DUMP_ENCODING_CACHE
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: ENCODING_CACHE_BYTESIZE => MULTIO_ENCODING_CACHE_BYTESIZE

  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: FREE   => MULTIO_ENCODER_FREE

END TYPE

!> Whitelist of public symbols
PUBLIC :: MULTIOM_CACHED_ENCODERS_T

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_ENCODER_INIT'
PP_THREAD_SAFE FUNCTION MULTIO_ENCODER_INIT( THIS, &
& MAPPING_FNAME, ENCODER_FNAME, METADATA, &
& CACHE_OPTIONS, ENCODER_OPTIONS, MAPPING_OPTIONS, FILTER_OPTIONS, &
& HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,    ONLY: JPIB_K
  USE :: HOOKS_MOD,            ONLY: HOOKS_T
  USE :: METADATA_BASE_MOD,    ONLY: METADATA_BASE_A
  USE :: METADATA_FACTORY_MOD, ONLY: MAKE_METADATA
  USE :: YAML_CORE_UTILS_MOD,  ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD,  ONLY: YAML_NEW_CONFIGURATION_FROM_FILE
  USE :: YAML_CORE_UTILS_MOD,  ONLY: YAML_DELETE_CONFIGURATION

  USE :: FILTER_OPTIONS_MOD,       ONLY: FILTER_OPTIONS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T
  USE :: MAPPING_OPTIONS_MOD,      ONLY: MAPPING_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(MULTIOM_CACHED_ENCODERS_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                 INTENT(IN)    :: MAPPING_FNAME
  CHARACTER(LEN=*),                 INTENT(IN)    :: ENCODER_FNAME
  CLASS(METADATA_BASE_A), POINTER,  INTENT(IN)    :: METADATA
  TYPE(CACHE_OPTIONS_T),            INTENT(IN)    :: CACHE_OPTIONS
  TYPE(GRIB_ENCODER_OPTIONS_T),     INTENT(IN)    :: ENCODER_OPTIONS
  TYPE(MAPPING_OPTIONS_T),          INTENT(IN)    :: MAPPING_OPTIONS
  TYPE(FILTER_OPTIONS_T),           INTENT(IN)    :: FILTER_OPTIONS
  TYPE(HOOKS_T),                    INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  TYPE(YAML_CONFIGURATION_T) :: MAPPING_CONFIG
  TYPE(YAML_CONFIGURATION_T) :: ENCODER_CONFIG
  LOGICAL :: FEXIST

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CLONE_METADATA = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAPPING_CFG_FILE_DOES_NOT_EXIST = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_CFG_FILE_DOES_NOT_EXIST = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAPPING_RULES_INIT = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_RULES_INIT = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAPPING_RULE_DELETE_ERROR = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_RULE_DELETE_ERROR = 7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAPPING_CACHE_INIT = 8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_CACHE_INIT = 9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOAD_MAPPING_FILE = 10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOAD_ENCODER_FILE = 11_JPIB_K

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

  ! Inquire file existence
  INQUIRE( FILE=TRIM(ADJUSTL(ENCODER_FNAME)), EXIST=FEXIST )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. FEXIST, ERRFLAG_ENCODER_CFG_FILE_DOES_NOT_EXIST )
  INQUIRE( FILE=TRIM(ADJUSTL(MAPPING_FNAME)), EXIST=FEXIST )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. FEXIST, ERRFLAG_MAPPING_CFG_FILE_DOES_NOT_EXIST )

  ! Copy options
  THIS%ENCODER_OPTIONS = ENCODER_OPTIONS
  THIS%FILTER_OPTIONS  = FILTER_OPTIONS
  THIS%CACHE_OPTIONS   = CACHE_OPTIONS
  THIS%MAPPING_OPTIONS = MAPPING_OPTIONS

  !> Open the configuration file
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CLONE_METADATA) MAKE_METADATA( METADATA, THIS%METADATA, HOOKS )


  !> Open the configuration file
  PP_TRYCALL(ERRFLAG_UNABLE_TO_LOAD_MAPPING_FILE) YAML_NEW_CONFIGURATION_FROM_FILE( &
& TRIM(ADJUSTL(MAPPING_FNAME)), MAPPING_CONFIG, HOOKS )

  !> Open the configuration file
  PP_TRYCALL(ERRFLAG_UNABLE_TO_LOAD_ENCODER_FILE) YAML_NEW_CONFIGURATION_FROM_FILE( &
&   TRIM(ADJUSTL(ENCODER_FNAME)), ENCODER_CONFIG, HOOKS )



  !>
  !> Read the YAML configurations

  !> Read the mapping rules
  PP_TRYCALL(ERRFLAG_MAPPING_RULES_INIT) THIS%MAPPING_RULES%INIT( &
&    MAPPING_CONFIG, THIS%FILTER_OPTIONS, HOOKS )

  !> Read the encoder configuration
  PP_TRYCALL(ERRFLAG_ENCODER_RULES_INIT) THIS%ENCODER_RULES%INIT( &
&   ENCODER_CONFIG, THIS%FILTER_OPTIONS, THIS%ENCODER_OPTIONS, HOOKS )


  !>
  !> Close the YAML configurations

  !> Deallocate section configuration
  PP_TRYCALL(ERRFLAG_ENCODER_RULE_DELETE_ERROR) YAML_DELETE_CONFIGURATION( &
&     ENCODER_CONFIG, HOOKS )

  !> Destroy the configuration object
  PP_TRYCALL(ERRFLAG_MAPPING_RULE_DELETE_ERROR) YAML_DELETE_CONFIGURATION( &
&     MAPPING_CONFIG, HOOKS )

  !> Initialize the mapping cache
  PP_TRYCALL(ERRFLAG_MAPPING_CACHE_INIT) THIS%MAPPING_CACHE%INIT( THIS%CACHE_OPTIONS, HOOKS )


  PP_TRYCALL(ERRFLAG_ENCODER_CACHE_INIT) THIS%ENCODER_CACHE%INIT( THIS%CACHE_OPTIONS, HOOKS )

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
    CASE (ERRFLAG_MAPPING_CFG_FILE_DOES_NOT_EXIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The mapping configuration file does not exist: '//TRIM(ADJUSTL(MAPPING_FNAME)) )
    CASE (ERRFLAG_ENCODER_CFG_FILE_DOES_NOT_EXIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The encoder configuration file does not exist: '//TRIM(ADJUSTL(ENCODER_FNAME)) )
    CASE (ERRFLAG_MAPPING_RULES_INIT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to initialize the mapping rules' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Mapping rules file name: "'//TRIM(ADJUSTL(MAPPING_FNAME))//'"' )
    CASE (ERRFLAG_ENCODER_RULES_INIT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to initialize the encoder rules' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoding rules file name: "'//TRIM(ADJUSTL(ENCODER_FNAME))//'"' )
    CASE (ERRFLAG_MAPPING_RULE_DELETE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to delete the mapping configuration' )
    CASE (ERRFLAG_ENCODER_RULE_DELETE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to delete the encoder configuration' )
    CASE (ERRFLAG_MAPPING_CACHE_INIT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to initialize the mapping cache' )
    CASE (ERRFLAG_ENCODER_CACHE_INIT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to initialize the encoder cache' )
    CASE (ERRFLAG_UNABLE_TO_LOAD_MAPPING_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to load the mapping configuration file' )
    CASE (ERRFLAG_UNABLE_TO_LOAD_ENCODER_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to load the encoder configuration file' )
    CASE (ERRFLAG_UNABLE_TO_CLONE_METADATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to clone the metadata' )
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


END FUNCTION MULTIO_ENCODER_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_ENCODER_ENCODE'
PP_THREAD_SAFE FUNCTION MULTIO_ENCODER_ENCODE( THIS, &
&    MSG, PAR, METADATA_LIST, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,             ONLY: JPIB_K
  USE :: HOOKS_MOD,                     ONLY: HOOKS_T
  USE :: METADATA_BASE_MOD,             ONLY: METADATA_BASE_A
  USE :: METADATA_FACTORY_MOD,          ONLY: DESTROY_METADATA
  USE :: PARAMETRIZATION_MOD,           ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD,           ONLY: FORTRAN_MESSAGE_T
  USE :: METADATA_LIST_MOD,             ONLY: METADATA_LIST_T
  USE :: CACHED_MAPPER_COLLECTION_MOD,  ONLY: CACHED_MAPPER_COLLECTION_T
  USE :: CACHED_ENCODER_COLLECTION_MOD, ONLY: CACHED_ENCODER_COLLECTION_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(MULTIOM_CACHED_ENCODERS_T), INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),          INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),          INTENT(IN)    :: PAR
  TYPE(METADATA_LIST_T),            INTENT(INOUT) :: METADATA_LIST
  TYPE(HOOKS_T),                    INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: ENCODING_DONE
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: MAPPING_SZ
  INTEGER(KIND=JPIB_K) :: ENCODER_SZ
  TYPE(CACHED_MAPPER_COLLECTION_T), POINTER :: MAPPER
  TYPE(CACHED_ENCODER_COLLECTION_T), POINTER :: ENCODERS
  TYPE(FORTRAN_MESSAGE_T) :: MAPPED_MSG
  TYPE(PARAMETRIZATION_T) :: MAPPED_PAR
  CLASS(METADATA_BASE_A), POINTER :: METADATA
  CHARACTER(LEN=256) :: MAPPING_TAG
  CHARACTER(LEN=256) :: MAPPING_NAME
  CHARACTER(LEN=256) :: ENCODER_TAG
  CHARACTER(LEN=256) :: ENCODER_NAME
  CHARACTER(LEN=:), ALLOCATABLE :: JSON
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAPPING_CACHE_INIT = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_CACHE_INIT = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FREE_MAPPED_MSG = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FREE_MAPPED_PAR = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAPPING_COLLECTION_EVAL = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_COLLECTION_ENCODE = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ADD_METADATA = 7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FREE_METADATA_LIST = 8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE = 9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MARS_TO_JSON = 10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DUMP_MSG = 11_JPIB_K

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

  !> Initialize the encoding dome
  PP_TRYCALL(ERRFLAG_FREE_METADATA_LIST) METADATA_LIST%FREE( HOOKS )

  !> Encode the message
  MAPPER => NULL()
  PP_TRYCALL(ERRFLAG_MAPPING_CACHE_INIT) THIS%MAPPING_CACHE%ACCESS_OR_CREATE( MSG, PAR, &
  & THIS%MAPPING_RULES, MAPPER, THIS%CACHE_OPTIONS, THIS%MAPPING_OPTIONS, THIS%FILTER_OPTIONS, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(MAPPER), ERRFLAG_MAPPING_CACHE_INIT )

  !> Read the size of the mappers
  PP_TRYCALL(ERRFLAG_ENCODER_CACHE_INIT) MAPPER%SIZE( MAPPING_SZ, HOOKS)

  !> Perform mapping
  DO I = 1, MAPPING_SZ

    !> Reset mapped message and parameter
    PP_TRYCALL(ERRFLAG_FREE_MAPPED_MSG) MAPPED_MSG%FREE( HOOKS )
    PP_TRYCALL(ERRFLAG_FREE_MAPPED_PAR) MAPPED_PAR%FREE( HOOKS )

    !> Perform the mapping
    PP_TRYCALL(ERRFLAG_MAPPING_COLLECTION_EVAL) MAPPER%EVAL( I, &
&     MSG, PAR, MAPPED_MSG, MAPPED_PAR, MAPPING_TAG, MAPPING_NAME, HOOKS )


!     !> Print Mapped message
!     PP_TRYCALL(ERRFLAG_MARS_TO_JSON) MAPPED_MSG%TO_JSON( JSON, HOOKS )
!     IF ( ALLOCATED(JSON) ) THEN
!       WRITE(*,'(A,A)') ' * MAPPED - MARS to JSON:   ', JSON
!       DEALLOCATE( JSON, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
!       PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
!     ELSE
!       WRITE(*,*) ' * MAPPED - MARS to JSON: ', 'NO JSON'
!     END IF

    !> Encode the message
    PP_TRYCALL(ERRFLAG_ENCODER_CACHE_INIT) THIS%ENCODER_CACHE%ACCESS_OR_CREATE( &
&    MAPPED_MSG, MAPPED_PAR, THIS%METADATA, THIS%ENCODER_RULES, ENCODERS, &
&    THIS%CACHE_OPTIONS, THIS%ENCODER_OPTIONS, THIS%FILTER_OPTIONS, HOOKS )

    !> Read the size of the encoders
    PP_TRYCALL(ERRFLAG_ENCODER_CACHE_INIT) ENCODERS%SIZE( ENCODER_SZ, THIS%ENCODER_OPTIONS, HOOKS )

    !> Perform the encoding
    DO J = 1, ENCODER_SZ

      !> Initialize the metadata
      METADATA => NULL()

      !> Perform the encoding
      PP_TRYCALL(ERRFLAG_ENCODER_COLLECTION_ENCODE) ENCODERS%PRINT( J, &
&       MAPPED_MSG, MAPPED_PAR, 6_JPIB_K, 0_JPIB_K, THIS%ENCODER_OPTIONS, HOOKS )

      ENCODING_DONE = .FALSE.
!       !> Perform the encoding
!       PP_TRYCALL(ERRFLAG_ENCODER_COLLECTION_ENCODE) ENCODERS%ENCODE( J, &
! &       MAPPED_MSG, MAPPED_PAR, ENCODER_TAG, ENCODER_NAME, METADATA, ENCODING_DONE, &
! &       THIS%ENCODER_OPTIONS, HOOKS )
!
      !> If encdong required/done then add the metadata the the list
      IF ( ENCODING_DONE ) THEN
        PP_TRYCALL(ERRFLAG_UNABLE_TO_ADD_METADATA) METADATA_LIST%PUSH( MAPPED_MSG, MAPPED_PAR, &
&            MAPPING_TAG, MAPPING_NAME, ENCODER_TAG, ENCODER_NAME, METADATA, HOOKS )
      ENDIF
    ENDDO


    !> Reset mapped message and parameter
    PP_TRYCALL(ERRFLAG_FREE_MAPPED_MSG) MAPPED_MSG%FREE( HOOKS )
    PP_TRYCALL(ERRFLAG_FREE_MAPPED_PAR) MAPPED_PAR%FREE( HOOKS )

  ENDDO

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
    CASE (ERRFLAG_MAPPING_CACHE_INIT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to initialize the mapping cache' )
    CASE (ERRFLAG_ENCODER_CACHE_INIT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to initialize the encoder cache' )
    CASE (ERRFLAG_FREE_MAPPED_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free the mapped message' )
    CASE (ERRFLAG_FREE_MAPPED_PAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free the mapped parameter' )
    CASE (ERRFLAG_MAPPING_COLLECTION_EVAL)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to evaluate the mapping collection' )
    CASE (ERRFLAG_ENCODER_COLLECTION_ENCODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to encode the message' )
    CASE (ERRFLAG_UNABLE_TO_ADD_METADATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to add the metadata to the list' )
    CASE (ERRFLAG_FREE_METADATA_LIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free the metadata list' )
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate the JSON' )
    CASE (ERRFLAG_MARS_TO_JSON)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert MARS to JSON' )
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


END FUNCTION MULTIO_ENCODER_ENCODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_DUMP_ENCODING_CACHE'
PP_THREAD_SAFE FUNCTION MULTIO_DUMP_ENCODING_CACHE( THIS, DUMP_PATH, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,    ONLY: JPIB_K
  USE :: HOOKS_MOD,            ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(MULTIOM_CACHED_ENCODERS_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                 INTENT(IN)    :: DUMP_PATH
  TYPE(HOOKS_T),                    INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODING_CACHE_DUMP_ERROR = 2_JPIB_K

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

  !> Print all the rules
  PP_TRYCALL(ERRFLAG_ENCODING_CACHE_DUMP_ERROR)  THIS%ENCODER_CACHE%DUMP( &
&   DUMP_PATH, &
&   THIS%CACHE_OPTIONS, &
&   THIS% ENCODER_OPTIONS, &
&   HOOKS )

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
    CASE (ERRFLAG_ENCODING_CACHE_DUMP_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to dump encoding cache' )
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


END FUNCTION MULTIO_DUMP_ENCODING_CACHE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_ENCODING_CACHE_BYTESIZE'
PP_THREAD_SAFE FUNCTION MULTIO_ENCODING_CACHE_BYTESIZE( THIS, BYTESIZE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,    ONLY: JPIB_K
  USE :: HOOKS_MOD,            ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(MULTIOM_CACHED_ENCODERS_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),             INTENT(OUT)   :: BYTESIZE
  TYPE(HOOKS_T),                    INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODING_BITESIZE_ERROR = 2_JPIB_K

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

  !> Print all the rules
  PP_TRYCALL( ERRFLAG_ENCODING_BITESIZE_ERROR ) THIS%ENCODER_CACHE%BYTESIZE( &
&   BYTESIZE, &
&   THIS%CACHE_OPTIONS, &
&   THIS% ENCODER_OPTIONS, &
&   HOOKS )

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
    CASE (ERRFLAG_ENCODING_BITESIZE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to compute the bytesize of the encoding cache' )
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


END FUNCTION MULTIO_ENCODING_CACHE_BYTESIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_ENCODER_PRINT'
PP_THREAD_SAFE FUNCTION MULTIO_ENCODER_PRINT( THIS, UNIT, OFFSET, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,    ONLY: JPIB_K
  USE :: HOOKS_MOD,            ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(MULTIOM_CACHED_ENCODERS_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),             INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),             INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),                    INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAPPING_RULE_PRINT_ERROR = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_RULE_PRINT_ERROR = 2_JPIB_K

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

  !> Print all the rules
  PP_TRYCALL( ERRFLAG_MAPPING_RULE_PRINT_ERROR ) THIS%MAPPING_RULES%PRINT( &
&                                 UNIT, OFFSET, HOOKS )
  PP_TRYCALL( ERRFLAG_ENCODER_RULE_PRINT_ERROR ) THIS%ENCODER_RULES%PRINT( &
&                                 UNIT, OFFSET, THIS%ENCODER_OPTIONS, HOOKS )

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
    CASE (ERRFLAG_MAPPING_RULE_PRINT_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to print the mapping rules' )
    CASE (ERRFLAG_ENCODER_RULE_PRINT_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to print the encoder rules' )
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


END FUNCTION MULTIO_ENCODER_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_ENCODER_FREE'
PP_THREAD_SAFE FUNCTION MULTIO_ENCODER_FREE( THIS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,    ONLY: JPIB_K
  USE :: HOOKS_MOD,            ONLY: HOOKS_T
  USE :: METADATA_FACTORY_MOD, ONLY: DESTROY_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(MULTIOM_CACHED_ENCODERS_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA_FREE = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_RULES_FREE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_CACHE_FREE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAPPING_RULES_FREE = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAPPING_CACHE_FREE = 5_JPIB_K

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

  ! Destroy the metadata
  PP_TRYCALL(ERRFLAG_METADATA_FREE) DESTROY_METADATA( THIS%METADATA, HOOKS )

  ! Deallocate all the rules
  PP_TRYCALL(ERRFLAG_ENCODER_CACHE_FREE) THIS%ENCODER_RULES%FREE( THIS%ENCODER_OPTIONS, HOOKS )
  PP_TRYCALL(ERRFLAG_ENCODER_RULES_FREE) THIS%ENCODER_CACHE%FREE( THIS%CACHE_OPTIONS, THIS%ENCODER_OPTIONS, HOOKS )

  !> Read the intop configuration
  PP_TRYCALL(ERRFLAG_MAPPING_RULES_FREE) THIS%MAPPING_RULES%FREE( HOOKS )
  PP_TRYCALL(ERRFLAG_MAPPING_CACHE_FREE) THIS%MAPPING_CACHE%FREE( THIS%CACHE_OPTIONS, THIS%MAPPING_OPTIONS, HOOKS )

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
    CASE (ERRFLAG_METADATA_FREE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free the metadata' )
    CASE (ERRFLAG_ENCODER_RULES_FREE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free the encoder rules' )
    CASE (ERRFLAG_ENCODER_CACHE_FREE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free the encoder cache' )
    CASE (ERRFLAG_MAPPING_RULES_FREE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free the mapping rules' )
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


END FUNCTION MULTIO_ENCODER_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE MULTIOM_CACHED_ENCODER_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME