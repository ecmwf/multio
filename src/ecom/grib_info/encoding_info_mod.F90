!> @file OM_CORE_MOD.F90
!>
!> @brief this file provide access to some grib informations
!>
!> This file provides acces to grib informations (loaded from input data)
!> needed to encode grib output. The module also provides utilities
!> to fast acces these iformations
!>
!> @todo add the possibility to read all the grib_info with one processor
!> and then broadcast. It is also possible to start reading from different
!> files according to the process ID, or read different files from different
!> processors and then perform an all-to-all communication instead of a
!> broadcast
!>
!> @author Mirco Valentini
!> @date January 31, 2024

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'encoding_info_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'ENCODING_INFO_MOD'
MODULE ENCODING_INFO_MOD

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,         ONLY: JPIB_K
  USE :: OM_CORE_MOD,         ONLY: GRIB_INFO_T
  USE :: MAP_MOD,             ONLY: MAP_T
  USE :: CIRCULAR_BUFFER_MOD, ONLY: CIRCULARBUFFER_T


IMPLICIT NONE

! Default visibility of the module
PRIVATE

! Encoding info
TYPE :: ENCODING_INFO_T
  TYPE(GRIB_INFO_T)      :: GRIB_INFO
  TYPE(CIRCULARBUFFER_T) :: TIME_HISTORY
END TYPE

!> @brief node in a list that contains pointer to circular buffers.
TYPE :: EI_LIST_NODE_T
  CLASS(ENCODING_INFO_T), POINTER :: EI_ => NULL()
  TYPE(EI_LIST_NODE_T),   POINTER :: NEXT_ => NULL()
  TYPE(EI_LIST_NODE_T),   POINTER :: PREV_ => NULL()
END TYPE

!> @brief List of circular buffers
TYPE :: EI_LIST_T
  TYPE(EI_LIST_NODE_T), POINTER :: HEAD_ => NULL()
  TYPE(EI_LIST_NODE_T), POINTER :: TAIL_ => NULL()
  INTEGER(KIND=JPIB_K) :: SIZE = 0
END TYPE

!> Encoding info object list
TYPE(EI_LIST_T) :: EI_LIST

! RB-Trees used to track level for different fields
CLASS(MAP_T), TARGET, ALLOCATABLE, DIMENSION(:,:,:) :: ENCODING_INFO

!  Whitelist of public symbols (variables)
PUBLIC :: ENCODING_INFO_T

!  Whitelist of public symbols (precedures)
PUBLIC :: SUENCODING_INFO
PUBLIC :: ENCODING_INFO_FREE
PUBLIC :: ENCODING_INFO_ACCESS_OR_CREATE
PUBLIC :: GRIB_INFO_PRINT
PUBLIC :: TRACK_TIME_PRINT

CONTAINS

!>
!> @brief function used to initialise the grib informations
!>
!> @see ENCODING_INFO_FREE
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SUENCODING_INFO'
SUBROUTINE SUENCODING_INFO( CFG, PROCESSOR_TOPO, MODEL_PARAMS, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_CORE_MOD,          ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,          ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,          ONLY: N_LEVTYPE_E
  USE :: OM_CORE_MOD,          ONLY: N_REPRES_E
  USE :: MAP_MOD,              ONLY: MAP_INIT
  USE :: YAML_RULES_MOD,       ONLY: INIT_RULES
  USE :: YAML_RULES_MOD,       ONLY: RULES_DIMS
  USE :: YAML_TIME_ASSUMPTIONS_MOD, ONLY: INIT_TIME_ASSUMPTION_RULES

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FCKIT_CONFIGURATION), INTENT(IN) :: CFG
  TYPE(PROC_TOPO_T),         INTENT(IN) :: PROCESSOR_TOPO
  TYPE(MODEL_PAR_T),         INTENT(IN) :: MODEL_PARAMS
  LOGICAL,                   INTENT(IN) :: VERBOSE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: NUM_PARAM_ID
  INTEGER(KIND=JPIB_K) :: MAX_PARAM_ID
  INTEGER(KIND=JPIB_K) :: MIN_PARAM_ID
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: K
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Read and initialise rules from YAML
  CALL INIT_RULES( CFG, VERBOSE )

  ! Read and initialise time encoding rules from YAML
  CALL INIT_TIME_ASSUMPTION_RULES( CFG, VERBOSE )

  ! Get information
  CALL RULES_DIMS( MIN_PARAM_ID, MAX_PARAM_ID, NUM_PARAM_ID )

  ! Allocate encoding info
  ALLOCATE( ENCODING_INFO(NUM_PARAM_ID,N_LEVTYPE_E,N_REPRES_E), STAT=STAT, ERRMSG=ERRMSG )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )

  ! Initialize all the maps
  DO I = 1, SIZE(ENCODING_INFO,1)
    DO J = 1, SIZE(ENCODING_INFO,2)
      DO K = 1, SIZE(ENCODING_INFO,3)
        CALL MAP_INIT( ENCODING_INFO(I,J,K) )
      ENDDO
    ENDDO
  ENDDO

  ! Initialise encoding tables
  CALL TIME_ENCODING_TABLE_HEADER( )
  CALL PACKING_ENCODING_TABLE_HEADER()

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
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate encoding_info: "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate encoding_info' )
      ENDIF
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

END SUBROUTINE SUENCODING_INFO
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief function used to get the grib informations given the grib
!>        index
!>
!> @param [in] grib index of the field
!>
!> @result pointer to the grib informations
!>
!> @see SUGRIB_INFO
!> @see ENCODING_INFO_FREE
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ENCODING_INFO_ACCESS_OR_CREATE'
SUBROUTINE ENCODING_INFO_ACCESS_OR_CREATE( MODEL_PARAMS, PARAM_ID, PREFIX, REPRES, LEVEL, EI )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,               ONLY: JPIB_K
  USE :: OM_CORE_MOD,               ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,               ONLY: LOOKUP_TABLE
  USE :: OM_CORE_MOD,               ONLY: OM_SET_CURRENT_GRIB_INFO
  USE :: OM_CORE_MOD,               ONLY: CAPACITY
  USE :: OM_CORE_MOD,               ONLY: GRIB_INFO_T
  USE :: MAP_MOD,                   ONLY: MAP_GET
  USE :: MAP_MOD,                   ONLY: MAP_INSERT
  USE :: MAP_MOD,                   ONLY: KEY_T
  USE :: YAML_RULES_MOD,            ONLY: MATCH_RULES
  USE :: YAML_RULES_MOD,            ONLY: DEFINITIONS_T
  USE :: MSG_UTILS_MOD,             ONLY: IPREFIX2ILEVTYPE
  USE :: YAML_TIME_ASSUMPTIONS_MOD, ONLY: MATCH_ASSUMPTIONS_RULES
  USE :: YAML_TIME_ASSUMPTIONS_MOD, ONLY: TIME_ASSUMPTIONS_T
  USE :: YAML_TIME_ASSUMPTIONS_MOD, ONLY: LEVEL_ASSUMPTIONS_T
  USE :: GENERAL_ASSUMPTIONS_MOD,   ONLY: IS_ENSAMBLE_SIMULATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),              INTENT(IN)  :: MODEL_PARAMS
  INTEGER(KIND=JPIB_K),           INTENT(IN)  :: PARAM_ID
  INTEGER(KIND=JPIB_K),           INTENT(IN)  :: PREFIX
  INTEGER(KIND=JPIB_K),           INTENT(IN)  :: REPRES
  INTEGER(KIND=JPIB_K),           INTENT(IN)  :: LEVEL
  TYPE(ENCODING_INFO_T), POINTER, INTENT(OUT) :: EI

  ! Local variables
  TYPE(TIME_ASSUMPTIONS_T)   :: TIME_ASSUMPTIONS
  TYPE(LEVEL_ASSUMPTIONS_T)  :: LEVEL_ASSUMPTIONS
  TYPE(GRIB_INFO_T), POINTER :: GRIB_INFO
  INTEGER(KIND=JPIB_K) :: IDX
  INTEGER(KIND=JPIB_K) :: LEV_TYPE
  TYPE(DEFINITIONS_T) :: DEFINITIONS
  TYPE(KEY_T) :: KEY
  CLASS(*), POINTER :: VALUE
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( PARAM_ID .GT. SIZE(LOOKUP_TABLE),  1 )
  PP_DEBUG_DEVELOP_COND_THROW( PARAM_ID .LT. 1,  2 )

  ! Search keys
  LEV_TYPE = IPREFIX2ILEVTYPE( PREFIX, PARAM_ID, LEVEL, REPRES )
  KEY%K = LEVEL

  ! Get the index from the paramID
  IDX = LOOKUP_TABLE( PARAM_ID )

  ! Get encoding info
  EX = MAP_GET( ENCODING_INFO(IDX,LEV_TYPE,REPRES), KEY, VALUE )

  IF ( EX ) THEN

    SELECT TYPE ( A => VALUE )

    CLASS IS ( ENCODING_INFO_T )

      ! Set output variables
      EI => A

      ! Add the curent grib info to the debug info
      CALL OM_SET_CURRENT_GRIB_INFO( A%GRIB_INFO, GRIB_INFO_PRINT )

    CLASS DEFAULT

      ! Error handling
      PP_DEBUG_DEVELOP_THROW( 4 )

    END SELECT

  ELSE

    ! Allocate new encoding info
    EI => ENCODING_INFO_NEW( )
    GRIB_INFO => EI%GRIB_INFO

    ! Extract definitions from encding rules
    CALL MATCH_RULES( PARAM_ID, LEV_TYPE, REPRES, LEVEL, DEFINITIONS )

    ! Time assumptions for the specified field
    CALL MATCH_ASSUMPTIONS_RULES( PARAM_ID, LEV_TYPE, REPRES, LEVEL, IS_ENSAMBLE_SIMULATION( MODEL_PARAMS ), TIME_ASSUMPTIONS, LEVEL_ASSUMPTIONS )

    ! Fill grib info
    GRIB_INFO%PRODUCT_DEFINITION_TEMPLATE_NUMBER_ = TIME_ASSUMPTIONS%PRODUCT_DEFINITION_TEMPLATE_NUMBER
    GRIB_INFO%TYPE_OF_STATISTICAL_PROCESS_        = TIME_ASSUMPTIONS%TYPE_OF_STATISTICAL_PROCESSING
    GRIB_INFO%TYPE_OF_TIME_RANGE_                 = TIME_ASSUMPTIONS%TYPE_OF_TIME_RANGE
    GRIB_INFO%OVERALL_LENGTH_OF_TIME_RANGE_       = TIME_ASSUMPTIONS%LENGTH_OF_TIME_RANGE_IN_SECONDS
    GRIB_INFO%IS_STEP0_VALID_                     = TIME_ASSUMPTIONS%EMIT_STEP_ZERO

    IF ( LEVEL_ASSUMPTIONS%CUSTOM_LEVELS_ENCODING ) THEN
      GRIB_INFO%CUSTOM_LEVELS_ENCODING = .TRUE.
      GRIB_INFO%TYPE_OF_FIRST_FIXED_SURFACE          = LEVEL_ASSUMPTIONS%TYPE_OF_FIRST_FIXED_SURFACE
      GRIB_INFO%SCALE_FACTOR_OF_FIRST_FIXED_SURFACE  = LEVEL_ASSUMPTIONS%SCALE_FACTOR_OF_FIRST_FIXED_SURFACE
      GRIB_INFO%SCALE_VALUE_OF_FIRST_FIXED_SURFACE   = LEVEL_ASSUMPTIONS%SCALE_VALUE_OF_FIRST_FIXED_SURFACE
      GRIB_INFO%TYPE_OF_SECOND_FIXED_SURFACE         = LEVEL_ASSUMPTIONS%TYPE_OF_SECOND_FIXED_SURFACE
      GRIB_INFO%SCALE_FACTOR_OF_SECOND_FIXED_SURFACE = LEVEL_ASSUMPTIONS%SCALE_FACTOR_OF_SECOND_FIXED_SURFACE
      GRIB_INFO%SCALE_VALUE_OF_SECOND_FIXED_SURFACE  = LEVEL_ASSUMPTIONS%SCALE_VALUE_OF_SECOND_FIXED_SURFACE
    ENDIF

    ! Initialize and create grib info (Grib info is created using the definitions extracted from the rules and assumptions)
    CALL INITIALIZE_GRIB_INFO( MODEL_PARAMS, PARAM_ID, PREFIX, LEV_TYPE, &
&            REPRES, LEVEL, DEFINITIONS, EI%GRIB_INFO )

    CALL PACKING_ENCODING_TABLE_ENTRY( PARAM_ID, REPRES, LEV_TYPE, LEVEL, &
&            PREFIX, GRIB_INFO%BITS_PER_VALUE, GRIB_INFO%PACKING_TYPE )

    CALL TIME_ENCODING_TABLE_ENTRY( PARAM_ID, REPRES, LEV_TYPE, LEVEL, PREFIX, EI%GRIB_INFO )

    ! Initialize and update time history
    CALL EI%TIME_HISTORY%INIT( CAPACITY )

    ! Match rules and assumptions
    VALUE => EI

    CALL MAP_INSERT( ENCODING_INFO(IDX,LEV_TYPE,REPRES), KEY, VALUE )

    ! Add the curent grib info to the debug info
    CALL OM_SET_CURRENT_GRIB_INFO( EI%GRIB_INFO, GRIB_INFO_PRINT )

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
    CHARACTER(LEN=128) :: CGRIB_ID

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      CGRIB_ID = REPEAT( ' ', 128 )
      WRITE(CGRIB_ID,'(I12)') PARAM_ID
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Grib_ID out of bounds. Greater than upper bound: '//TRIM(ADJUSTL(CGRIB_ID)) )
    CASE (2)
      CGRIB_ID = REPEAT( ' ', 128 )
      WRITE(CGRIB_ID,'(I12)') PARAM_ID
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Grib_ID out of bounds. Lower than lower bound: '//TRIM(ADJUSTL(CGRIB_ID)) )
    CASE (3)
      CGRIB_ID = REPEAT( ' ', 128 )
      WRITE(CGRIB_ID,'(I12)') PARAM_ID
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Grib info not associated to the grib_ID: '//TRIM(ADJUSTL(CGRIB_ID)) )
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

END SUBROUTINE ENCODING_INFO_ACCESS_OR_CREATE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'INITIALIZE_GRIB_INFO'
SUBROUTINE INITIALIZE_GRIB_INFO( MODEL_PARAMS, PARAM_ID, PREFIX, LEV_TYPE, REPRES, LEVEL, DEFINITIONS, GRIB_INFO )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,             ONLY: JPIB_K
  USE :: OM_CORE_MOD,             ONLY: GRIB_INFO_T
  USE :: OM_CORE_MOD,             ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,             ONLY: LEVTYPE_SOL_E
  USE :: OM_CORE_MOD,             ONLY: UNDEF_PARAM_E
  USE :: YAML_RULES_MOD,          ONLY: DEFINITIONS_T

  USE :: PACKAGING_ASSUMPTIONS_MOD, ONLY: COMPUTE_BITS_PER_VALUE
  USE :: PACKAGING_ASSUMPTIONS_MOD, ONLY: COMPUTE_PACKING_TYPE
  USE :: LEVEL_ASSUMPTIONS_MOD,     ONLY: COMPUTE_TOPBOT
  ! USE :: TIME_ASSUMPTIONS_MOD,      ONLY: COMPUTE_TIMING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),     INTENT(IN)    :: MODEL_PARAMS
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: PARAM_ID
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: PREFIX
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: LEV_TYPE
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: REPRES
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: LEVEL
  TYPE(DEFINITIONS_T),   INTENT(IN)    :: DEFINITIONS
  TYPE(GRIB_INFO_T),     INTENT(INOUT) :: GRIB_INFO

  ! Local variables
  INTEGER(KIND=JPIB_K) :: TMP

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Configure Encoding type (using definitions from YAML)
  GRIB_INFO%REQUESTED_ENCODING = DEFINITIONS%EDITION

  ! Set levType
  GRIB_INFO%ILEV_TYPE = LEV_TYPE

  ! Configure Packing
  SELECT CASE (DEFINITIONS%BITS_PER_VALUE)

  CASE (-1)
    ! Default table uncompressed
    GRIB_INFO%BITS_PER_VALUE = COMPUTE_BITS_PER_VALUE( MODEL_PARAMS, PARAM_ID, PREFIX, REPRES, .FALSE. )
  CASE (-2)
    ! Default table compressed
    GRIB_INFO%BITS_PER_VALUE = COMPUTE_BITS_PER_VALUE( MODEL_PARAMS, PARAM_ID, PREFIX, REPRES, .TRUE. )
  CASE (1:64)
    ! Costom configuration from input file
    GRIB_INFO%BITS_PER_VALUE = DEFINITIONS%BITS_PER_VALUE
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( 1 )
  END SELECT

  ! Packing type
  IF ( DEFINITIONS%PACKING_TYPE .NE. UNDEF_PARAM_E ) THEN
    GRIB_INFO%PACKING_TYPE = DEFINITIONS%PACKING_TYPE
  ELSE
    GRIB_INFO%PACKING_TYPE = COMPUTE_PACKING_TYPE( MODEL_PARAMS, PARAM_ID, REPRES )
  ENDIF

  ! Direct to FDB flag
  GRIB_INFO%DIRECT_TO_FDB = DEFINITIONS%DIRECT_TO_FDB


  ! Configure Levels (using definitions from YAML file and hardcoded assuptions)
  IF ( LEV_TYPE .EQ. LEVTYPE_SOL_E ) THEN
    CALL COMPUTE_TOPBOT( MODEL_PARAMS, PARAM_ID, GRIB_INFO%ITOP_, GRIB_INFO%IBOT_ )
  ENDIF

! GRIB_INFO%IS_STEP0_VALID_ = .FALSE.

!   CALL COMPUTE_TIMING( &
! &  MODEL_PARAMS, &
! &  PARAM_ID, &
! &  GRIB_INFO%IS_STEP0_VALID_, &
! &  GRIB_INFO%PRODUCT_DEFINITION_TEMPLATE_NUMBER_, &
! &  GRIB_INFO%TYPE_OF_STATISTICAL_PROCESS_, &
! &  GRIB_INFO%TYPE_OF_TIME_RANGE_, &
! &  GRIB_INFO%OVERALL_LENGTH_OF_TIME_RANGE_ )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'invalid value for bits per value' )
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

END SUBROUTINE INITIALIZE_GRIB_INFO
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODING_INFO_NEW'
FUNCTION ENCODING_INFO_NEW() RESULT(PEI)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Local variables
  CLASS(ENCODING_INFO_T), POINTER :: PEI

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialisation
  NULLIFY( PEI )

  ! Allocation
  IF ( .NOT. ASSOCIATED(EI_LIST%HEAD_) ) THEN
    ALLOCATE( EI_LIST%HEAD_ )
    EI_LIST%TAIL_ => EI_LIST%HEAD_
    ALLOCATE( EI_LIST%HEAD_%EI_ )
    PEI => EI_LIST%TAIL_%EI_
    EI_LIST%SIZE = 1
    NULLIFY(EI_LIST%HEAD_%PREV_)
    NULLIFY(EI_LIST%HEAD_%NEXT_)
  ELSE
    ALLOCATE( EI_LIST%TAIL_%NEXT_ )
    EI_LIST%TAIL_%NEXT_%PREV_ => EI_LIST%TAIL_
    EI_LIST%TAIL_ => EI_LIST%TAIL_%NEXT_
    NULLIFY(EI_LIST%TAIL_%NEXT_)
    ALLOCATE( EI_LIST%TAIL_%EI_ )
    PEI => EI_LIST%TAIL_%EI_
    EI_LIST%SIZE = EI_LIST%SIZE + 1
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION ENCODING_INFO_NEW
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ENCODING_INFO_FREE_ALL'
SUBROUTINE ENCODING_INFO_FREE_ALL()

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Local variables
  TYPE(EI_LIST_NODE_T), POINTER :: CURR

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( ASSOCIATED(EI_LIST%HEAD_) ) THEN

    DO

      IF ( .NOT.ASSOCIATED(EI_LIST%TAIL_) ) THEN
        EXIT
      ENDIF

      CURR => EI_LIST%TAIL_
      EI_LIST%TAIL_ => EI_LIST%TAIL_%PREV_
      IF ( ASSOCIATED(EI_LIST%TAIL_) ) THEN
        NULLIFY(EI_LIST%TAIL_%NEXT_)
      ENDIF

      CALL CURR%EI_%TIME_HISTORY%FREE()
      DEALLOCATE( CURR%EI_ )
      ! Paranoid operation
      NULLIFY( CURR%EI_ )
      DEALLOCATE( CURR )
      NULLIFY(CURR)

      EI_LIST%SIZE = EI_LIST%SIZE - 1

    ENDDO

    NULLIFY(EI_LIST%TAIL_)
    NULLIFY(EI_LIST%HEAD_)
    EI_LIST%SIZE = 0

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE ENCODING_INFO_FREE_ALL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

!>
!> @brief free all the grib informations
!>
!> @param [in] param IDX of the file to be loaded
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ENCODING_INFO_FREE'
SUBROUTINE ENCODING_INFO_FREE(  )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,               ONLY: JPIB_K
  USE :: MAP_MOD,                   ONLY: MAP_FREE
  USE :: YAML_RULES_MOD,            ONLY: FREE_RULES
  USE :: YAML_TIME_ASSUMPTIONS_MOD, ONLY: FREE_TIME_ASSUMPTION_RULES
  ! USE :: TIME_ASSUMPTIONS_MOD,      ONLY: TIME_ASSUMPTIONS_FREE
  USE :: PACKAGING_ASSUMPTIONS_MOD, ONLY: PACKAGING_ASSUMPTIONS_FREE
  USE :: LEVEL_ASSUMPTIONS_MOD,     ONLY: LEVEL_ASSUMPTIONS_FREE
  USE :: GENERAL_ASSUMPTIONS_MOD,   ONLY: GENERAL_ASSUMPTIONS_FREE

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: K

  ! Closing encoding tables
  CALL TIME_ENCODING_TABLE_FOOTER()
  CALL PACKING_ENCODING_TABLE_FOOTER()

  ! Free all the encoding info list
  CALL ENCODING_INFO_FREE_ALL()

  ! Free all the rules from YAML
  CALL FREE_RULES()

  ! Free all the rules from YAML
  CALL FREE_TIME_ASSUMPTION_RULES()

  ! Initialize all the maps
  DO I = 1, SIZE(ENCODING_INFO,1)
    DO J = 1, SIZE(ENCODING_INFO,2)
      DO K = 1, SIZE(ENCODING_INFO,3)
        CALL MAP_FREE( ENCODING_INFO(I,J,K) )
      ENDDO
    ENDDO
  ENDDO

  ! Free the lookup tables
  IF ( ALLOCATED(ENCODING_INFO) ) THEN
    DEALLOCATE(ENCODING_INFO)
  ENDIF

  ! Free assumptions configuration
  ! CALL TIME_ASSUMPTIONS_FREE()
  CALL LEVEL_ASSUMPTIONS_FREE()
  CALL PACKAGING_ASSUMPTIONS_FREE()
  CALL GENERAL_ASSUMPTIONS_FREE()

  ! Exit point on error
  RETURN

END SUBROUTINE ENCODING_INFO_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ENCODING_INFO_PRINT'
SUBROUTINE GRIB_INFO_PRINT( GI, LOGUNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_CORE_MOD,          ONLY: GRIB_INFO_T
  USE :: OM_CORE_MOD,          ONLY: PACKING_TYPE_GRIB_SIMPLE_E
  USE :: OM_CORE_MOD,          ONLY: PACKING_TYPE_GRIB_CCSDE_E
  USE :: OM_CORE_MOD,          ONLY: PACKING_TYPE_GRIB_COMPLEX_E
  USE :: TIME_ASSUMPTIONS_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_TO_STRING
  USE :: TIME_ASSUMPTIONS_MOD, ONLY: TYPE_OF_TIME_RANGE_TO_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(GRIB_INFO_T),    INTENT(IN) :: GI
  INTEGER(KIND=JPIB_K), INTENT(IN) :: LOGUNIT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  WRITE(LOGUNIT,'(A)') ' '
  WRITE(LOGUNIT,'(A)') ' GRIB INFO'
  WRITE(LOGUNIT,'(A)') ' ---------'
  WRITE(LOGUNIT,'(A,7I8)') ' + Requested encoding......................: ', GI%REQUESTED_ENCODING
  WRITE(LOGUNIT,'(A,7I8)') ' + Bits per values.........................: ', GI%BITS_PER_VALUE
  WRITE(LOGUNIT,'(A,7I8)') ' + (I) Packing type........................: ', GI%PACKING_TYPE
  SELECT CASE (GI%PACKING_TYPE)
  CASE ( PACKING_TYPE_GRIB_SIMPLE_E )
    WRITE(LOGUNIT,'(A)')     ' + (C) Packing type........................: grib_simple'
  CASE ( PACKING_TYPE_GRIB_CCSDE_E )
    WRITE(LOGUNIT,'(A)')     ' + (C) Packing type........................: grib_ccsds'
  CASE ( PACKING_TYPE_GRIB_COMPLEX_E )
    WRITE(LOGUNIT,'(A)')     ' + (C) Packing type........................: spectral_complex'
  END SELECT
  WRITE(LOGUNIT,'(A,L)')   ' + Is step 0 valid.........................: ', GI%IS_STEP0_VALID_
  WRITE(LOGUNIT,'(A,I8)')  ' + Product definition template number......: ', GI%PRODUCT_DEFINITION_TEMPLATE_NUMBER_
  WRITE(LOGUNIT,'(A,A)')   ' + Type of statistical process.............: ', TRIM(TYPE_OF_STATISTICAL_PROCESS_TO_STRING(GI%TYPE_OF_STATISTICAL_PROCESS_))
  WRITE(LOGUNIT,'(A,A)')   ' + Type of timerange.......................: ', TRIM(TYPE_OF_TIME_RANGE_TO_STRING(GI%TYPE_OF_TIME_RANGE_))
  WRITE(LOGUNIT,'(A,I8)')  ' + Overall length of timerange.............: ', GI%OVERALL_LENGTH_OF_TIME_RANGE_
  WRITE(LOGUNIT,'(A,I8)')  ' + Itop....................................: ', GI%IBOT_
  WRITE(LOGUNIT,'(A,I8)')  ' + Ibot....................................: ', GI%ITOP_

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE GRIB_INFO_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TRACK_TIME_PRINT'
SUBROUTINE TRACK_TIME_PRINT( TIME_HISTORY, LOGUNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: TIME_HISTORY_T

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(TIME_HISTORY_T), INTENT(INOUT) :: TIME_HISTORY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: LOGUNIT

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  WRITE(LOGUNIT,'(A)') ' '
  WRITE(LOGUNIT,'(A)') ' TIME HISTORY (steps)'
  WRITE(LOGUNIT,'(A)') ' --------------------'
  WRITE(LOGUNIT,'(A)', ADVANCE='NO') ' + '
  DO I = 1, TIME_HISTORY%SIZE_ - 1
    WRITE(LOGUNIT,'(I8,A)', ADVANCE='NO') TIME_HISTORY%HIST_(I), ', '
  ENDDO
  WRITE(LOGUNIT,'(I8)') TIME_HISTORY%HIST_(TIME_HISTORY%SIZE_)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE TRACK_TIME_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TIME_ENCODING_TABLE_HEADER'
SUBROUTINE TIME_ENCODING_TABLE_HEADER( )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: UNIT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  OPEN( NEWUNIT=UNIT, FILE='timeManagement.log', ACTION='WRITE', STATUS='REPLACE' )
  WRITE(UNIT,'(A)')    '# This file has been automatically geneated by:'
  WRITE(UNIT,'(A,A)')  '#  -> file........... ::', PP_FILE_NAME
  WRITE(UNIT,'(A,A)')  '#  -> sectionType.... ::', PP_SECTION_TYPE
  WRITE(UNIT,'(A,A)')  '#  -> sectionName.... ::', PP_SECTION_NAME
  WRITE(UNIT,'(A,A)')  '#  -> procedureType.. ::', PP_PROCEDURE_TYPE
  WRITE(UNIT,'(A,A)')  '#  -> procedureName.. ::', PP_PROCEDURE_NAME
  WRITE(UNIT,'(A,I8)') '#  -> line........... ::', __LINE__
  WRITE(UNIT,'(A)')    '# It is intended just for debugging purposes and not used to configure anything.'
  WRITE(UNIT,'(A)')    '# Please do not edit this file.'
  WRITE(UNIT,'(A)')    '#  '
  WRITE(UNIT,'(A)')    '#  PARAM_ID     :: Grib parameter Identifier'
  WRITE(UNIT,'(A)')    '#  REPRES       :: Grib representation (gridded, gpherical harmonics)'
  WRITE(UNIT,'(A)')    '#  LEV_TYPE     :: Grib levType'
  WRITE(UNIT,'(A)')    '#  LEVEL        :: Grib level'
  WRITE(UNIT,'(A)')    '#  MODEL        :: Model that generated the message (atm., wave., ...)'
  WRITE(UNIT,'(A)')    '#  P.D.T. (t=0) :: Product Definition Template number for step 0'
  WRITE(UNIT,'(A)')    '#  T.S.P. (t=0) :: Type of Statistical Process for step 0'
  WRITE(UNIT,'(A)')    '#  T.T.R. (t=0) :: Type of Time Range for step 0'
  WRITE(UNIT,'(A)')    '#  L.T.R. (t=0) :: Length of Time Range in seconds for step 0'
  WRITE(UNIT,'(A)')    '#  EMIT STEP 0  :: True if step 0 has to be emitted'
  WRITE(UNIT,'(A)')    '#  P.D.T.       :: Product Definition Template number'
  WRITE(UNIT,'(A)')    '#  T.S.P.       :: Type of Statistical Process'
  WRITE(UNIT,'(A)')    '#  T.T.R.       :: Type of Time Range'
  WRITE(UNIT,'(A)')    '#  L.T.R.       :: Length of Time Range in seconds'
  WRITE(UNIT,'(A)')    '#  '
  WRITE(UNIT,'(A,A)')  '#', REPEAT('-',171)
  WRITE(UNIT,'(A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1)') '|', &
&  ' PARAM ID.      ', '|', &
&  ' REPRES         ', '|', &
&  ' LEV_TYPE       ', '|', &
&  ' LEVEL          ', '|', &
&  ' MODEL          ', '|', &
&  ' EMIT STEP 0    ', '|', &
&  ' P.D.T.         ', '|', &
&  ' T.S.P.         ', '|', &
&  ' T.T.R.         ', '|', &
&  ' L.T.R. [s]     ', '|'
  WRITE(UNIT,'(A,A)')  '#', REPEAT('-',171)
  CLOSE(UNIT)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE TIME_ENCODING_TABLE_HEADER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TIME_ENCODING_TABLE_ENTRY'
SUBROUTINE TIME_ENCODING_TABLE_ENTRY( PARAM_ID, REPRES, LEV_TYPE, LEVEL, PREFIX, GRIB_INFO )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,   ONLY: JPIB_K
  USE :: OM_CORE_MOD,   ONLY: GRIB_INFO_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),  INTENT(IN) :: PARAM_ID
  INTEGER(KIND=JPIB_K),  INTENT(IN) :: REPRES
  INTEGER(KIND=JPIB_K),  INTENT(IN) :: LEV_TYPE
  INTEGER(KIND=JPIB_K),  INTENT(IN) :: LEVEL
  INTEGER(KIND=JPIB_K),  INTENT(IN) :: PREFIX
  TYPE(GRIB_INFO_T), INTENT(IN) :: GRIB_INFO

  ! Local variables
  INTEGER(KIND=JPIB_K) :: UNIT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  OPEN( NEWUNIT=UNIT, FILE='timeManagement.log', ACTION='WRITE', POSITION='APPEND', STATUS='OLD' )
  WRITE(UNIT,'(A1,I16,A1,A16,A1,A16,A1,I16,A1,A16,A1,L16,A1,I16,A1,I16,A1,I16,A1,I16,A1)') '|', &
&  PARAM_ID, '|', &
&  IREPRES2CREPRES(REPRES),   '|', &
&  ILEVTYPE2CLEVTYPE(LEV_TYPE), '|', &
&  LEVEL,    '|', &
&  IPREF2MODEL(PREFIX), '|', &
&  GRIB_INFO%IS_STEP0_VALID_, '|', &
&  GRIB_INFO%PRODUCT_DEFINITION_TEMPLATE_NUMBER_, '|', &
&  GRIB_INFO%TYPE_OF_STATISTICAL_PROCESS_, '|', &
&  GRIB_INFO%TYPE_OF_TIME_RANGE_, '|', &
&  GRIB_INFO%OVERALL_LENGTH_OF_TIME_RANGE_, '|'
  CLOSE(UNIT)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE TIME_ENCODING_TABLE_ENTRY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TIME_ENCODING_TABLE_FOOTER'
SUBROUTINE TIME_ENCODING_TABLE_FOOTER( )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,   ONLY: JPIB_K
  USE :: OM_CORE_MOD,   ONLY: GRIB_INFO_T
  USE :: MSG_UTILS_MOD, ONLY: IPREF2MSGTYPE

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: UNIT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  OPEN( NEWUNIT=UNIT, FILE='timeManagement.log', ACTION='WRITE', POSITION='APPEND', STATUS='OLD' )
  WRITE(UNIT,'(A,A)')  '#', REPEAT('-',171)
  CLOSE(UNIT)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE TIME_ENCODING_TABLE_FOOTER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PACKING_ENCODING_TABLE_HEADER'
SUBROUTINE PACKING_ENCODING_TABLE_HEADER( )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: UNIT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  OPEN( NEWUNIT=UNIT, FILE='bitsPerValue.log', ACTION='WRITE', STATUS='REPLACE' )
  WRITE(UNIT,'(A)')    '# This file has been automatically geneated by:'
  WRITE(UNIT,'(A,A)')  '#  -> file........... ::', PP_FILE_NAME
  WRITE(UNIT,'(A,A)')  '#  -> sectionType.... ::', PP_SECTION_TYPE
  WRITE(UNIT,'(A,A)')  '#  -> sectionName.... ::', PP_SECTION_NAME
  WRITE(UNIT,'(A,A)')  '#  -> procedureType.. ::', PP_PROCEDURE_TYPE
  WRITE(UNIT,'(A,A)')  '#  -> procedureName.. ::', PP_PROCEDURE_NAME
  WRITE(UNIT,'(A,I8)') '#  -> line........... ::', __LINE__
  WRITE(UNIT,'(A)')    '# It is intended just for debugging purposes and not used to configure anything.'
  WRITE(UNIT,'(A)')    '# Please do not edit this file.'
  WRITE(UNIT,'(A)')    '#  '
  WRITE(UNIT,'(A)')    '#  PARAM_ID       :: Grib parameter Identifier'
  WRITE(UNIT,'(A)')    '#  REPRES         :: Grib representation (gridded, gpherical harmonics)'
  WRITE(UNIT,'(A)')    '#  LEV_TYPE       :: Grib levType'
  WRITE(UNIT,'(A)')    '#  LEVEL          :: Grib level'
  WRITE(UNIT,'(A)')    '#  MODEL          :: Model that generated the message (atm., wave., ...)'
  WRITE(UNIT,'(A)')    '#  BITS_PER_VALUE :: Model level'
  WRITE(UNIT,'(A)')    '#  PACKING TYPE   :: Packing type'
  WRITE(UNIT,'(A)')    '#  '
  WRITE(UNIT,'(A,A)')  '#',REPEAT('-',124)
  WRITE(UNIT,'(A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1,A20,A1)') '|', &
&  ' PARAM ID.      ', '|', &
&  ' REPRES         ', '|', &
&  ' LEV_TYPE       ', '|', &
&  ' LEVEL          ', '|', &
&  ' MODEL          ', '|', &
&  ' BITS PER VALUE ', '|', &
&  ' PACKING_TYPE       ', '|'
  WRITE(UNIT,'(A,A)')  '#',REPEAT('-',124)
  CLOSE(UNIT)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE PACKING_ENCODING_TABLE_HEADER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PACKING_ENCODING_TABLE_ENTRY'
SUBROUTINE PACKING_ENCODING_TABLE_ENTRY( PARAM_ID, REPRES, LEV_TYPE, LEVEL, PREFIX, BITS_PER_VALUE, PACKING_TYPE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,   ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),  INTENT(IN) :: PARAM_ID
  INTEGER(KIND=JPIB_K),  INTENT(IN) :: REPRES
  INTEGER(KIND=JPIB_K),  INTENT(IN) :: LEV_TYPE
  INTEGER(KIND=JPIB_K),  INTENT(IN) :: LEVEL
  INTEGER(KIND=JPIB_K),  INTENT(IN) :: PREFIX
  INTEGER(KIND=JPIB_K),  INTENT(IN) :: BITS_PER_VALUE
  INTEGER(KIND=JPIB_K),  INTENT(IN) :: PACKING_TYPE


  ! Local variables
  INTEGER(KIND=JPIB_K) :: UNIT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  OPEN( NEWUNIT=UNIT, FILE='bitsPerValue.log', ACTION='WRITE', POSITION='APPEND', STATUS='OLD' )
  WRITE(UNIT,'(A1,I16,A1,A16,A1,A16,A1,I16,A1,A16,A1,I16,A1,A20,A1)') '|', &
&  PARAM_ID, '|', &
&  IREPRES2CREPRES(REPRES),   '|', &
&  ILEVTYPE2CLEVTYPE(LEV_TYPE), '|', &
&  LEVEL,    '|', &
&  IPREF2MODEL(PREFIX), '|', &
&  BITS_PER_VALUE, '|', &
&  IPACKINGTYPE2CPACKINGTYPE(PACKING_TYPE), '|'
  CLOSE(UNIT)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE PACKING_ENCODING_TABLE_ENTRY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PACKING_ENCODING_TABLE_FOOTER'
SUBROUTINE PACKING_ENCODING_TABLE_FOOTER( )

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: UNIT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  OPEN( NEWUNIT=UNIT, FILE='bitsPerValue.log', ACTION='WRITE', POSITION='APPEND', STATUS='OLD' )
  WRITE(UNIT,'(A,A)')  '#',REPEAT('-',124)
  CLOSE(UNIT)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE PACKING_ENCODING_TABLE_FOOTER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


FUNCTION IPACKINGTYPE2CPACKINGTYPE( IPACKINGTYPE ) RESULT( CPACKINGTYPE )
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: PACKING_TYPE_GRIB_SIMPLE_E
  USE :: OM_CORE_MOD, ONLY: PACKING_TYPE_GRIB_CCSDE_E
  USE :: OM_CORE_MOD, ONLY: PACKING_TYPE_GRIB_COMPLEX_E
IMPLICIT NONE
  INTEGER(KIND=JPIB_K),  INTENT(IN) :: IPACKINGTYPE
  CHARACTER(LEN=20) :: CPACKINGTYPE
  SELECT CASE(IPACKINGTYPE)

  CASE (PACKING_TYPE_GRIB_SIMPLE_E)
    CPACKINGTYPE='grib_simple'
  CASE (PACKING_TYPE_GRIB_CCSDE_E)
    CPACKINGTYPE='grib_ccsds'
  CASE (PACKING_TYPE_GRIB_COMPLEX_E)
    CPACKINGTYPE='spectral_complex'
  CASE DEFAULT
    CPACKINGTYPE='--'
  END SELECT
END FUNCTION IPACKINGTYPE2CPACKINGTYPE

FUNCTION IREPRES2CREPRES( IREPRES ) RESULT( CREPRES )
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: REPRES_GRIDDED_E
  USE :: OM_CORE_MOD, ONLY: REPRES_SPECTRAL_E
IMPLICIT NONE
  INTEGER(KIND=JPIB_K),  INTENT(IN) :: IREPRES
  CHARACTER(LEN=2) :: CREPRES
  SELECT CASE(IREPRES)

  CASE (REPRES_GRIDDED_E)
    CREPRES='gg'
  CASE (REPRES_SPECTRAL_E)
    CREPRES='sh'
  CASE DEFAULT
    CREPRES='--'
  END SELECT
END FUNCTION IREPRES2CREPRES


FUNCTION IPREF2MODEL( IPREF ) RESULT( CMODEL )
  USE :: OM_CORE_MOD,   ONLY: JPIB_K
  USE :: OM_CORE_MOD,   ONLY: ATM_MSG_E
  USE :: OM_CORE_MOD,   ONLY: WAM_MSG_E
  USE :: MSG_UTILS_MOD, ONLY: IPREF2MSGTYPE
IMPLICIT NONE
  INTEGER(KIND=JPIB_K),  INTENT(IN) :: IPREF
  CHARACTER(LEN=3) :: CMODEL
  SELECT CASE(IPREF2MSGTYPE(IPREF))

  CASE (ATM_MSG_E)
    CMODEL='atm'
  CASE (WAM_MSG_E)
    CMODEL='wam'
  CASE DEFAULT
    CMODEL='--'
  END SELECT
END FUNCTION IPREF2MODEL

FUNCTION ILEVTYPE2CLEVTYPE( ILEVTYPE ) RESULT( CLEVTYPE )
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
IMPLICIT NONE
  INTEGER(KIND=JPIB_K),  INTENT(IN) :: ILEVTYPE
   CHARACTER(LEN=3) :: CLEVTYPE
  SELECT CASE(ILEVTYPE)

  CASE (LEVTYPE_HHL_E)
    CLEVTYPE='hhl'
  CASE (LEVTYPE_HPL_E)
    CLEVTYPE='hlp'
  CASE (LEVTYPE_HL_E)
    CLEVTYPE='hl'
  CASE (LEVTYPE_ML_E)
    CLEVTYPE='ml'
  CASE (LEVTYPE_O2D_E)
    CLEVTYPE='o2d'
  CASE (LEVTYPE_O3D_E)
    CLEVTYPE='o3d'
  CASE (LEVTYPE_PL_E)
    CLEVTYPE='pl'
  CASE (LEVTYPE_PT_E)
    CLEVTYPE='pt'
  CASE (LEVTYPE_PV_E)
    CLEVTYPE='pv'
  CASE (LEVTYPE_SFC_E)
    CLEVTYPE='sfc'
  CASE (LEVTYPE_SOL_E)
    CLEVTYPE='sol'
  CASE DEFAULT
    CLEVTYPE='---'
  END SELECT
END FUNCTION ILEVTYPE2CLEVTYPE


END MODULE ENCODING_INFO_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
