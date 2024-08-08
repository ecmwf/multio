! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'yaml_assumptions_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'YAML_TIME_ASSUMPTIONS_MOD'
MODULE YAML_TIME_ASSUMPTIONS_MOD

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: UNDEF_PARAM_E

IMPLICIT NONE

! Default visibility of the module
PRIVATE


!> Rules used to match fields
TYPE :: MATCHER_T
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE :: LEV_TYPE
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE :: LEVEL
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE :: REPRES
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE :: PARAM_ID
  LOGICAL :: IS_ENSAMBLE = .FALSE.
END TYPE


!> Configurations loaded from yaml
TYPE :: TIME_ASSUMPTIONS_T
  INTEGER(KIND=JPIB_K) :: PRODUCT_DEFINITION_TEMPLATE_NUMBER=UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: TYPE_OF_TIME_RANGE=UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: LENGTH_OF_TIME_RANGE_IN_SECONDS=UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: TYPE_OF_STATISTICAL_PROCESSING=UNDEF_PARAM_E
  LOGICAL :: EMIT_STEP_ZERO=.FALSE.
END TYPE

!> Configurations loaded from yaml
TYPE :: LEVEL_ASSUMPTIONS_T
  LOGICAL :: CUSTOM_LEVELS_ENCODING = .FALSE.

  INTEGER(KIND=JPIB_K) :: TYPE_OF_FIRST_FIXED_SURFACE=UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: SCALE_FACTOR_OF_FIRST_FIXED_SURFACE=UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: SCALE_VALUE_OF_FIRST_FIXED_SURFACE=UNDEF_PARAM_E

  INTEGER(KIND=JPIB_K) :: TYPE_OF_SECOND_FIXED_SURFACE=UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: SCALE_FACTOR_OF_SECOND_FIXED_SURFACE=UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: SCALE_VALUE_OF_SECOND_FIXED_SURFACE=UNDEF_PARAM_E

END TYPE


!> Container for rules
TYPE :: RULE_T
  CHARACTER(LEN=128)        :: NAME
  TYPE(MATCHER_T)           :: MATCHER
  TYPE(TIME_ASSUMPTIONS_T)  :: TIME_ASSUMPTIONS
  TYPE(LEVEL_ASSUMPTIONS_T) :: LEVEL_ASSUMPTIONS
END TYPE

!> Default assumptions
TYPE(RULE_T), TARGET, DIMENSION(:), ALLOCATABLE :: DEFAULT_ASSUMPTIONS
TYPE(RULE_T), TARGET, DIMENSION(:), ALLOCATABLE :: SPECIAL_ASSUMPTIONS


!> Whitelise of public symbols
PUBLIC :: TIME_ASSUMPTIONS_T
PUBLIC :: LEVEL_ASSUMPTIONS_T
PUBLIC :: INIT_TIME_ASSUMPTION_RULES
PUBLIC :: FREE_TIME_ASSUMPTION_RULES
PUBLIC :: MATCH_ASSUMPTIONS_RULES

CONTAINS

#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'INIT'
SUBROUTINE INIT_TIME_ASSUMPTION_RULES( MAIN_CFG, VERBOSE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_REPLACE_ENVVAR_IN_STRING

  ! Symbols imported from other libraries
  USE :: FCKIT_PATHNAME_MODULE,      ONLY: FCKIT_PATHNAME
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_YAMLCONFIGURATION
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: DEALLOCATE_FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FCKIT_CONFIGURATION), INTENT(IN) :: MAIN_CFG
  LOGICAL,                   INTENT(IN) :: VERBOSE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  TYPE(FCKIT_CONFIGURATION) :: CFG
  TYPE(FCKIT_CONFIGURATION) :: ASSUMPTIONS_RULES_CFG
  TYPE(FCKIT_CONFIGURATION), ALLOCATABLE, DIMENSION(:) :: DEFAULT_RULES_CFG
  TYPE(FCKIT_CONFIGURATION), ALLOCATABLE, DIMENSION(:) :: SPECIAL_RULES_CFG
  CHARACTER(LEN=:), ALLOCATABLE :: CLTMP
  CHARACTER(LEN=1024) :: YAMLFNAME
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  YAMLFNAME = REPEAT(' ',1024)
  IF ( MAIN_CFG%GET( 'time-assumptions-file-name', CLTMP ) ) THEN
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CLTMP), 0 )
    CALL OM_REPLACE_ENVVAR_IN_STRING( CLTMP, YAMLFNAME )
  ELSE
    YAMLFNAME = './time-assumptions.yaml'
  ENDIF

  ! Free memory
  IF ( ALLOCATED(CLTMP) ) THEN
    DEALLOCATE(CLTMP)
  ENDIF

  ! Check if the file exists
  WRITE(*,*) 'Reading file: ', TRIM(ADJUSTL(YAMLFNAME))
  INQUIRE(FILE=YAMLFNAME, EXIST=EX)
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, 1 )

  ! Open the time assumptions file
  CFG = FCKIT_YAMLCONFIGURATION( FCKIT_PATHNAME( TRIM(YAMLFNAME) ) )

  ! Reading the encoding rules
  IF ( CFG%GET( 'assumptions-rules', ASSUMPTIONS_RULES_CFG ) ) THEN

    ! Read the specific encoding rules
    IF ( ASSUMPTIONS_RULES_CFG%GET( 'default-rules', DEFAULT_RULES_CFG ) ) THEN
      ! Logging
      IF ( VERBOSE ) THEN
        WRITE(ERROR_UNIT,*) 'The size of the allocated object is: ', SIZE(DEFAULT_RULES_CFG)
      ENDIF

      ! Memory allocation
      ALLOCATE(DEFAULT_ASSUMPTIONS(SIZE(DEFAULT_RULES_CFG)), STAT=STAT, ERRMSG=ERRMSG )
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )

      ! Loop over the rules and read rule configuration
      DO I = 1, SIZE(DEFAULT_RULES_CFG)
        ! Logging
        IF ( VERBOSE ) THEN
          WRITE(ERROR_UNIT,*) 'Reading the default next rule: ', I
        ENDIF
        CALL READ_NAME( DEFAULT_RULES_CFG(I), DEFAULT_ASSUMPTIONS(I)%NAME, VERBOSE )
        CALL READ_ACTIONS( DEFAULT_RULES_CFG(I), DEFAULT_ASSUMPTIONS(I), VERBOSE )
      ENDDO

      ! Log rules names
      IF ( VERBOSE ) THEN
        WRITE(ERROR_UNIT,*) DEFAULT_ASSUMPTIONS(1)%NAME
      ENDIF
    ENDIF

    ! Read the specific encoding rules
    IF ( ASSUMPTIONS_RULES_CFG%GET( 'special-rules', SPECIAL_RULES_CFG ) ) THEN

      ! Logging
      IF ( VERBOSE ) THEN
        WRITE(ERROR_UNIT,*) 'The size of the allocated object is: ', SIZE(SPECIAL_RULES_CFG)
      ENDIF

      ! Memory allocation
      ALLOCATE(SPECIAL_ASSUMPTIONS(SIZE(SPECIAL_RULES_CFG)), STAT=STAT, ERRMSG=ERRMSG )
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )

      ! Loop over the rules and read rule configuration
      DO I = 1, SIZE(SPECIAL_RULES_CFG)
        ! Logging
        IF ( VERBOSE ) THEN
          WRITE(ERROR_UNIT,*) 'Reading the next special rule: ', I
        ENDIF
        CALL READ_NAME( SPECIAL_RULES_CFG(I), SPECIAL_ASSUMPTIONS(I)%NAME, VERBOSE )
        CALL READ_ACTIONS( SPECIAL_RULES_CFG(I), SPECIAL_ASSUMPTIONS(I), VERBOSE )
      ENDDO

      ! Log rules names
      IF ( VERBOSE ) THEN
        WRITE(ERROR_UNIT,*) SPECIAL_ASSUMPTIONS(1)%NAME
      ENDIF

    ENDIF

    ! Free rules array
    CALL DEALLOCATE_FCKIT_CONFIGURATION( DEFAULT_RULES_CFG )
    CALL DEALLOCATE_FCKIT_CONFIGURATION( SPECIAL_RULES_CFG )

  ENDIF

  ! Free memory
  CALL ASSUMPTIONS_RULES_CFG%FINAL()

  ! Destroy the fckit configuration object
  CALL CFG%FINAL()

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=128) :: TMP

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (0)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Time assumptions file name not allocated after reading' )
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'file does not exist' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate rules: '//TRIM(ADJUSTL(ERRMSG)) )
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

END SUBROUTINE INIT_TIME_ASSUMPTION_RULES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'FREE_TIME_ASSUMPTION_RULES'
SUBROUTINE FREE_TIME_ASSUMPTION_RULES()

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: LOOKUP_TABLE
  USE :: OM_CORE_MOD, ONLY: LOOKUP_TABLE_BWD

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( ALLOCATED(SPECIAL_ASSUMPTIONS) ) THEN

    ! Deallocate rules components
    DO I = 1, SIZE(SPECIAL_ASSUMPTIONS)
      IF ( ALLOCATED(SPECIAL_ASSUMPTIONS(I)%MATCHER%LEV_TYPE) ) THEN
        DEALLOCATE(SPECIAL_ASSUMPTIONS(I)%MATCHER%LEV_TYPE, STAT=STAT, ERRMSG=ERRMSG)
        PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )
      ENDIF
      IF ( ALLOCATED(SPECIAL_ASSUMPTIONS(I)%MATCHER%LEVEL) ) THEN
        DEALLOCATE(SPECIAL_ASSUMPTIONS(I)%MATCHER%LEVEL, STAT=STAT, ERRMSG=ERRMSG)
        PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )
      ENDIF
      IF ( ALLOCATED(SPECIAL_ASSUMPTIONS(I)%MATCHER%REPRES) ) THEN
        DEALLOCATE(SPECIAL_ASSUMPTIONS(I)%MATCHER%REPRES, STAT=STAT, ERRMSG=ERRMSG)
        PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 3 )
      ENDIF
      IF ( ALLOCATED(SPECIAL_ASSUMPTIONS(I)%MATCHER%PARAM_ID) ) THEN
        DEALLOCATE(SPECIAL_ASSUMPTIONS(I)%MATCHER%PARAM_ID, STAT=STAT, ERRMSG=ERRMSG)
        PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 4 )
      ENDIF
    ENDDO

    ! deallocate rules array
    DEALLOCATE( SPECIAL_ASSUMPTIONS, STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 5 )

  ENDIF

  IF ( ALLOCATED(LOOKUP_TABLE) ) THEN
    DEALLOCATE(LOOKUP_TABLE, STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 6 )
  ENDIF

  IF ( ALLOCATED(LOOKUP_TABLE_BWD) ) THEN
    DEALLOCATE(LOOKUP_TABLE_BWD, STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 7 )
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
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate levType: "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate levType' )
      ENDIF
    CASE (2)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate level: "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate level' )
      ENDIF
    CASE (3)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate representation: "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate representation' )
      ENDIF
    CASE (4)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate paramId: "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate paramId' )
      ENDIF
    CASE (5)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate rules: "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate rules' )
      ENDIF
    CASE (6)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate "lookup_table": "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate "lookup_table"' )
      ENDIF
    CASE (7)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate "lookup_table_bwd": "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate "lookup_table_bwd"' )
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


END SUBROUTINE FREE_TIME_ASSUMPTION_RULES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_NAME'
SUBROUTINE READ_NAME( CFG, NAME, VERBOSE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FCKIT_CONFIGURATION), INTENT(IN)  :: CFG
  CHARACTER(LEN=*),          INTENT(OUT) :: NAME
  LOGICAL,                   INTENT(IN)  :: VERBOSE

  ! Local variables
  CHARACTER(LEN=:), ALLOCATABLE :: CLTMP
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Read rule's name
  NAME = REPEAT(' ',LEN(NAME))
  IF ( CFG%GET( 'rule', CLTMP  ) ) THEN
    IF ( ALLOCATED(CLTMP) ) THEN

      ! Check name length
      PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(CLTMP).GT.LEN(NAME), 1 )

      ! Store the rule name
      NAME = CLTMP(:)
      IF ( VERBOSE ) THEN
        WRITE(ERROR_UNIT,*) 'Rule name: ', TRIM(ADJUSTL(NAME))
      ENDIF

      ! Free temporary string
      DEALLOCATE(CLTMP, STAT=STAT, ERRMSG=ERRMSG )
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )

    ELSE
      PP_DEBUG_CRITICAL_THROW( 3 )
    ENDIF
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
    CHARACTER(LEN=128) :: TMP1
    CHARACTER(LEN=128) :: TMP2

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      TMP1=REPEAT(' ',128)
      TMP2=REPEAT(' ',128)
      WRITE(TMP1,'(I10)') LEN(NAME)
      WRITE(TMP2,'(I10)') LEN_TRIM(CLTMP)
      PP_DEBUG_CREATE_ERROR_MSG( STR, '"name" '//TRIM(CLTMP)//'is too long, maximum length is: '//TRIM(TMP1)//', passed length is: '//TRIM(TMP2) )
    CASE (2)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate temporay "name": "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate temporary "name"' )
      ENDIF
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Rules temporary string for "name" not allocated after succesful YAML reading' )
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

END SUBROUTINE READ_NAME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_ACTIONS'
SUBROUTINE READ_ACTIONS( CFG, RULE, VERBOSE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: UNDEF_PARAM_E

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FCKIT_CONFIGURATION), INTENT(IN)    :: CFG
  TYPE(RULE_T),              INTENT(INOUT) :: RULE
  LOGICAL,                   INTENT(IN)    :: VERBOSE

  ! Local variables
  TYPE(FCKIT_CONFIGURATION) :: FILTER_CFG
  TYPE(FCKIT_CONFIGURATION) :: TIME_ASSUMPTION_CFG
  TYPE(FCKIT_CONFIGURATION) :: LEVEL_ASSUMPTION_CFG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Read filters
  IF ( CFG%GET( 'filter', FILTER_CFG ) ) THEN

    ! Logging
    IF ( VERBOSE ) THEN
      WRITE(ERROR_UNIT,*) 'Read filter'
    ENDIF

    ! Read filters
    CALL READ_FILTER( FILTER_CFG, RULE%MATCHER, VERBOSE )

    ! Deallocate YAML filters
    CALL FILTER_CFG%FINAL()

  ELSE

    PP_DEBUG_CRITICAL_THROW( 1 )

  ENDIF

  ! Read encoding options
  IF ( CFG%GET( 'time-assumption', TIME_ASSUMPTION_CFG ) ) THEN

    ! Logging
    IF ( VERBOSE ) THEN
      WRITE(ERROR_UNIT,*) 'Read encode'
    ENDIF

    ! Read encdoing options
    CALL READ_TIME_ASSUMPTION( TIME_ASSUMPTION_CFG, RULE%TIME_ASSUMPTIONS, VERBOSE )

    ! Deallocate YAML encoding options
    CALL TIME_ASSUMPTION_CFG%FINAL()

  ELSE

    PP_DEBUG_CRITICAL_THROW( 2 )

  ENDIF

  ! Read encoding options
  IF ( CFG%GET( 'level-assumption', LEVEL_ASSUMPTION_CFG ) ) THEN

    ! Logging
    IF ( VERBOSE ) THEN
      WRITE(ERROR_UNIT,*) 'Read encode'
    ENDIF

    ! Read encdoing options
    CALL READ_LEVEL_ASSUMPTION( LEVEL_ASSUMPTION_CFG, RULE%LEVEL_ASSUMPTIONS, VERBOSE )

    ! Deallocate YAML encoding options
    CALL LEVEL_ASSUMPTION_CFG%FINAL()

  ELSE

    RULE%LEVEL_ASSUMPTIONS%CUSTOM_LEVELS_ENCODING = .FALSE.
    RULE%LEVEL_ASSUMPTIONS%TYPE_OF_FIRST_FIXED_SURFACE = UNDEF_PARAM_E
    RULE%LEVEL_ASSUMPTIONS%SCALE_FACTOR_OF_FIRST_FIXED_SURFACE = UNDEF_PARAM_E
    RULE%LEVEL_ASSUMPTIONS%SCALE_VALUE_OF_FIRST_FIXED_SURFACE = UNDEF_PARAM_E
    RULE%LEVEL_ASSUMPTIONS%TYPE_OF_SECOND_FIXED_SURFACE = UNDEF_PARAM_E
    RULE%LEVEL_ASSUMPTIONS%SCALE_FACTOR_OF_SECOND_FIXED_SURFACE = UNDEF_PARAM_E
    RULE%LEVEL_ASSUMPTIONS%SCALE_VALUE_OF_SECOND_FIXED_SURFACE = UNDEF_PARAM_E

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to find filters' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to find encoding definition' )
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

END SUBROUTINE READ_ACTIONS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_FILTER'
SUBROUTINE READ_FILTER( CFG, MATCHER, VERBOSE)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,   ONLY: JPIB_K
  USE :: MSG_UTILS_MOD, ONLY: CLEVTYPE2ILEVTYPE

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FCKIT_CONFIGURATION), INTENT(IN)    :: CFG
  TYPE(MATCHER_T),           INTENT(INOUT) :: MATCHER
  LOGICAL,                   INTENT(IN)    :: VERBOSE

  ! Local variables
  LOGICAL :: LTMP
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: LO
  INTEGER(KIND=JPIB_K) :: HI
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: SZ
  INTEGER(KIND=JPIB_K) :: ITMP
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  CHARACTER(LEN=:), ALLOCATABLE :: CLTMP
  CHARACTER(LEN=:), ALLOCATABLE, DIMENSION(:) :: ATMP


  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()


  ! Read rules for levtype
  IF ( CFG%GET( 'levtype', ATMP  ) ) THEN

    IF ( ALLOCATED(ATMP) ) THEN

      ! Allocate levtype rule
      ALLOCATE(MATCHER%LEV_TYPE(SIZE(ATMP)), STAT=STAT, ERRMSG=ERRMSG )
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )

      ! Convert levtype to integer and store it in the rule
      DO I = 1, SIZE(ATMP)
        MATCHER%LEV_TYPE(I) = CLEVTYPE2ILEVTYPE( ATMP(I) )
        IF ( VERBOSE ) THEN
          WRITE(ERROR_UNIT,*) 'levtype: ', I, MATCHER%LEV_TYPE(I), ATMP(I), SIZE(MATCHER%LEV_TYPE)
        ENDIF
      ENDDO

      ! Free the temporary string array
      DEALLOCATE(ATMP, STAT=STAT, ERRMSG=ERRMSG )
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )

    ELSE
      PP_DEBUG_CRITICAL_THROW( 3 )
    ENDIF

  ENDIF



  ! Read rules for representation
  IF ( CFG%GET( 'repres', ATMP  ) ) THEN

    IF ( ALLOCATED(ATMP) ) THEN

      ! Allocate repres rule
      ALLOCATE(MATCHER%REPRES(SIZE(ATMP)), STAT=STAT, ERRMSG=ERRMSG )
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 4)

      ! Convert repres to integer and store it in the rule
      DO I = 1, SIZE(ATMP)
        MATCHER%REPRES(I) = CREPRES2IREPRES( ATMP(I) )
        IF ( VERBOSE ) THEN
          WRITE(ERROR_UNIT,*) 'repres: ', I, MATCHER%REPRES(I), ATMP(I), SIZE(MATCHER%REPRES)
        ENDIF
      ENDDO

      ! Free the temporary string array
      DEALLOCATE(ATMP, STAT=STAT, ERRMSG=ERRMSG )
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 5 )

    ELSE
      PP_DEBUG_CRITICAL_THROW( 6 )
    ENDIF

  ENDIF


  ! Read rules for level
  IF ( CFG%GET( 'level', ATMP  ) ) THEN

    IF ( ALLOCATED(ATMP) ) THEN

      ! Allocate level rule
      ALLOCATE( MATCHER%LEVEL(SIZE(ATMP)), STAT=STAT, ERRMSG=ERRMSG )
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 7 )

      ! Store level in the rule
      DO I = 1, SIZE(ATMP)
        PP_DEBUG_CRITICAL_COND_THROW( .NOT.IS_INTEGER( ATMP(I) ), 8 )
        READ(ATMP(I),*) ITMP
        MATCHER%LEVEL(I) = ITMP
        IF ( VERBOSE ) THEN
          WRITE(ERROR_UNIT,*) 'level: ', I, MATCHER%LEVEL(I), ATMP(I), SIZE(MATCHER%LEVEL)
        ENDIF
      ENDDO

      ! Free the temporary string array
      DEALLOCATE(ATMP, STAT=STAT, ERRMSG=ERRMSG )
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 9 )

    ELSE
      PP_DEBUG_CRITICAL_THROW( 10 )
    ENDIF

  ENDIF


  ! Read rules for paramId
  IF ( CFG%GET( 'paramId', ATMP  ) ) THEN

    IF ( ALLOCATED(ATMP) ) THEN

      ! Count elements
      CNT = 0
      DO I = 1, SIZE(ATMP)
        IF ( IS_INTEGER( ATMP(I) ) ) THEN
          CNT = CNT + 1
        ELSEIF ( IS_INTEGER_RANGE( ATMP(I) ) ) THEN
          CALL READ_INTEGER_RANGE( ATMP(I), LO, HI )
          CNT = CNT + HI - LO + 1
        ELSE
          PP_DEBUG_CRITICAL_THROW( 12 )
        ENDIF
      ENDDO

      ! Allocate level rule
      ALLOCATE( MATCHER%PARAM_ID(CNT), STAT=STAT, ERRMSG=ERRMSG )
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 11 )

      ! Store paramId in the rule
      CNT = 0
      DO I = 1, SIZE(ATMP)
        IF ( IS_INTEGER( ATMP(I) ) ) THEN
          READ(ATMP(I),*) ITMP
          CNT = CNT + 1
          MATCHER%PARAM_ID(CNT) = ITMP
        ELSEIF ( IS_INTEGER_RANGE( ATMP(I) ) ) THEN
          CALL READ_INTEGER_RANGE( ATMP(I), LO, HI )
          DO J = LO, HI
            CNT = CNT + 1
            MATCHER%PARAM_ID(CNT) = J
          ENDDO
        ELSE
          PP_DEBUG_CRITICAL_THROW( 12 )
        ENDIF

        IF ( VERBOSE ) THEN
          WRITE(ERROR_UNIT,*) 'paramId: ', I, MATCHER%PARAM_ID(I) , ATMP(I), SIZE(MATCHER%PARAM_ID)
        ENDIF
      ENDDO

      ! Free the temporary string array
      DEALLOCATE(ATMP, STAT=STAT, ERRMSG=ERRMSG )
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 13 )

    ELSE
      PP_DEBUG_CRITICAL_THROW( 14 )
    ENDIF

  ENDIF


  ! Read rules for paramId
  IF ( CFG%GET( 'is-ensamble', MATCHER%IS_ENSAMBLE  ) ) THEN
      IF ( VERBOSE ) THEN
          WRITE(ERROR_UNIT,*) 'is-ensamble: ', MATCHER%IS_ENSAMBLE
      ENDIF
  ELSE
      MATCHER%IS_ENSAMBLE = .FALSE.
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
    CHARACTER(LEN=128) :: TMP

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate rule for "levType": "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate rules for "levType"' )
      ENDIF
    CASE (2)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate rules temporary string for "levType": "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate rules temporary string for "levType"' )
      ENDIF
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Rules temporary string for "levType" not allocated after succesful YAML reading' )

    CASE (4)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate rule for "repres": "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate rules for "repres"' )
      ENDIF
    CASE (5)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate rules temporary string for "repres": "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate rules temporary string for "repres"' )
      ENDIF
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Rules temporary string for "repres" not allocated after succesful YAML reading' )


    CASE (7)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate rule for "level": "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate rules for "level"' )
      ENDIF
    CASE (8)
      TMP=REPEAT(' ',128)
      WRITE(TMP,'(I10)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Element: '//TRIM(TMP)//' in the "level" array is not an integer: '//TRIM(ATMP(I)) )
    CASE (9)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate rules temporary string for "level": "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate rules temporary string for "level"' )
      ENDIF
    CASE (10)
      TMP=REPEAT(' ',128)
      WRITE(TMP,'(I8)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Rules temporary string for "level" not allocated after succesful YAML reading' )


    CASE (11)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate rule for "paramId": "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate rules for "paramId"' )
      ENDIF
    CASE (12)
      TMP=REPEAT(' ',128)
      WRITE(TMP,'(I10)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Element: '//TRIM(TMP)//' in the "paramId" array is not an integer: '//TRIM(ATMP(I)) )
    CASE (13)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate rules temporary string for "paramId": "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate rules temporary string for "paramId"' )
      ENDIF
    CASE (14)
      TMP=REPEAT(' ',128)
      WRITE(TMP,'(I8)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Rules temporary string for "paramId" not allocated after succesful YAML reading' )
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


END SUBROUTINE READ_FILTER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_TIME_ASSUMPTION'
SUBROUTINE READ_TIME_ASSUMPTION( CFG, TIME_ASSUMPTION, VERBOSE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_INSTANT_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FROM_STEP0_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FROM_LASTPP_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FIXED_SIZE_E
  USE :: OM_CORE_MOD, ONLY: UNDEF_PARAM_E

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FCKIT_CONFIGURATION), INTENT(IN)  :: CFG
  TYPE(TIME_ASSUMPTIONS_T),  INTENT(OUT) :: TIME_ASSUMPTION
  LOGICAL,                   INTENT(IN)  :: VERBOSE

  ! Local variables
  CHARACTER(LEN=:), ALLOCATABLE :: CLTMP
  INTEGER(KIND=JPIB_K) :: ITMP
  LOGICAL :: LTMP
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Read the product definition template number
  CALL READ_PRODUCT_DEFINITION_TEMPLATE_NUMBER_CFG( CFG, TIME_ASSUMPTION, VERBOSE )

  ! Read the type of time range
  CALL READ_TYPE_OF_TIME_RANGE_CFG( CFG, TIME_ASSUMPTION, VERBOSE )

  ! Read the type of statistical process
  CALL READ_TYPE_OF_STATISTICAL_PROCESS_CFG( CFG, TIME_ASSUMPTION, VERBOSE )

  ! Read the length of time range
  IF ( CFG%HAS( 'length-of-time-range-in-seconds' ) ) THEN
    CALL READ_LENGTH_OF_TIME_RANGE_IN_SECONDS_CFG( CFG, TIME_ASSUMPTION, VERBOSE )
  ELSEIF ( CFG%HAS( 'length-of-time-range-in-hours' ) ) THEN
    CALL READ_LENGTH_OF_TIME_RANGE_IN_HOURS_CFG( CFG, TIME_ASSUMPTION, VERBOSE )
  ELSE
    SELECT CASE (TIME_ASSUMPTION%TYPE_OF_TIME_RANGE)
    CASE (TYPE_OF_TIME_RANGE_INSTANT_E)
      TIME_ASSUMPTION%LENGTH_OF_TIME_RANGE_IN_SECONDS = 0
    CASE (TYPE_OF_TIME_RANGE_FROM_STEP0_E)
      TIME_ASSUMPTION%LENGTH_OF_TIME_RANGE_IN_SECONDS = -1
    CASE (TYPE_OF_TIME_RANGE_FROM_LASTPP_E)
      TIME_ASSUMPTION%LENGTH_OF_TIME_RANGE_IN_SECONDS = -2
    CASE DEFAULT
      PP_DEBUG_CRITICAL_THROW( 1 )
    END SELECT
  ENDIF

  ! Read the flag needed to emit the step 0
  CALL READ_EMIT_STEP_ZERO_CFG( CFG, TIME_ASSUMPTION, VERBOSE )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'expected keyword "lenght-of-time-range-<*>"' )
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

END SUBROUTINE READ_TIME_ASSUMPTION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_PRODUCT_DEFINITION_TEMPLATE_NUMBER_CFG'
SUBROUTINE READ_PRODUCT_DEFINITION_TEMPLATE_NUMBER_CFG( CFG, TIME_ASSUMPTION, VERBOSE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FCKIT_CONFIGURATION), INTENT(IN)    :: CFG
  TYPE(TIME_ASSUMPTIONS_T),  INTENT(INOUT) :: TIME_ASSUMPTION
  LOGICAL,                   INTENT(IN)    :: VERBOSE

  ! Local variables
  CHARACTER(LEN=:), ALLOCATABLE :: CLTMP
  INTEGER(KIND=JPIB_K) :: ITMP
  LOGICAL :: LTMP
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( CFG%GET( 'product-definition-template-number', ITMP  ) ) THEN
    TIME_ASSUMPTION%PRODUCT_DEFINITION_TEMPLATE_NUMBER = ITMP
    IF ( ITMP .NE. 0   .AND. &
&        ITMP .NE. 1   .AND. &
&        ITMP .NE. 8   .AND. &
&        ITMP .NE. 11  .AND. &
&        ITMP .NE. 99  .AND. &
&        ITMP .NE. 100 ) THEN
      PP_DEBUG_CRITICAL_THROW( 0 )
    ENDIF
    IF ( VERBOSE ) THEN
      WRITE(ERROR_UNIT,*) 'ProductDefinitionTemplateNumber: ',  TIME_ASSUMPTION%PRODUCT_DEFINITION_TEMPLATE_NUMBER
    ENDIF
  ELSE
    PP_DEBUG_CRITICAL_THROW( 1 )
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
    CHARACTER(LEN=32) :: CPDT

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (0)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unrecognized ProductDefinitionTemplateNumber' )
    CASE (1)
      CPDT = REPEAT(' ',32)
      WRITE(CPDT,*) ITMP
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Invalid value for "product-definition-template-number", expected one of [0,1,8,11], got: '//TRIM(ADJUSTL(CPDT)) )
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

END SUBROUTINE READ_PRODUCT_DEFINITION_TEMPLATE_NUMBER_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_LEVEL_ASSUMPTION'
SUBROUTINE READ_LEVEL_ASSUMPTION( CFG, LEVEL_ASSUMPTION, VERBOSE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: UNDEF_PARAM_E

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FCKIT_CONFIGURATION), INTENT(IN)  :: CFG
  TYPE(LEVEL_ASSUMPTIONS_T), INTENT(OUT) :: LEVEL_ASSUMPTION
  LOGICAL,                   INTENT(IN)  :: VERBOSE

  ! Local variables
  LOGICAL :: EX
  LOGICAL, DIMENSION(6) :: CHECKS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Conditions
  CHECKS(1) = CFG%HAS( 'type-of-first-fixed-surface' )
  CHECKS(2) = CFG%HAS( 'scale-factor-of-first-fixed-surface' )
  CHECKS(3) = CFG%HAS( 'scale-value-of-first-fixed-surface' )
  CHECKS(4) = CFG%HAS( 'type-of-second-fixed-surface' )
  CHECKS(5) = CFG%HAS( 'scale-factor-of-second-fixed-surface' )
  CHECKS(6) = CFG%HAS( 'scale-value-of-second-fixed-surface' )

  IF ( ALL(CHECKS) ) THEN
    LEVEL_ASSUMPTION%CUSTOM_LEVELS_ENCODING = .TRUE.
    EX = CFG%GET( 'type-of-first-fixed-surface', LEVEL_ASSUMPTION%TYPE_OF_FIRST_FIXED_SURFACE )
    EX = CFG%GET( 'scale-factor-of-first-fixed-surface', LEVEL_ASSUMPTION%SCALE_FACTOR_OF_FIRST_FIXED_SURFACE )
    EX = CFG%GET( 'scale-value-of-first-fixed-surface', LEVEL_ASSUMPTION%SCALE_VALUE_OF_FIRST_FIXED_SURFACE )
    EX = CFG%GET( 'type-of-second-fixed-surface', LEVEL_ASSUMPTION%TYPE_OF_SECOND_FIXED_SURFACE )
    EX = CFG%GET( 'scale-factor-of-second-fixed-surface', LEVEL_ASSUMPTION%SCALE_FACTOR_OF_SECOND_FIXED_SURFACE )
    EX = CFG%GET( 'scale-value-of-second-fixed-surface', LEVEL_ASSUMPTION%SCALE_VALUE_OF_SECOND_FIXED_SURFACE )
  ELSEIF ( ALL( .NOT.CHECKS ) ) THEN
    LEVEL_ASSUMPTION%CUSTOM_LEVELS_ENCODING = .FALSE.
    LEVEL_ASSUMPTION%TYPE_OF_FIRST_FIXED_SURFACE = UNDEF_PARAM_E
    LEVEL_ASSUMPTION%SCALE_FACTOR_OF_FIRST_FIXED_SURFACE = UNDEF_PARAM_E
    LEVEL_ASSUMPTION%SCALE_VALUE_OF_FIRST_FIXED_SURFACE = UNDEF_PARAM_E
    LEVEL_ASSUMPTION%TYPE_OF_SECOND_FIXED_SURFACE = UNDEF_PARAM_E
    LEVEL_ASSUMPTION%SCALE_FACTOR_OF_SECOND_FIXED_SURFACE = UNDEF_PARAM_E
    LEVEL_ASSUMPTION%SCALE_VALUE_OF_SECOND_FIXED_SURFACE = UNDEF_PARAM_E
  ELSE
    PP_DEBUG_DEVELOP_THROW( 1 )
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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'wrong configuration for levels' )
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

END SUBROUTINE READ_LEVEL_ASSUMPTION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_TYPE_OF_TIME_RANGE_CFG'
SUBROUTINE READ_TYPE_OF_TIME_RANGE_CFG( CFG, TIME_ASSUMPTION, VERBOSE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_INSTANT_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FROM_STEP0_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FROM_LASTPP_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FIXED_SIZE_E
  USE :: OM_GENERAL_UTILS_MOD, ONLY: TOLOWER

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FCKIT_CONFIGURATION), INTENT(IN)    :: CFG
  TYPE(TIME_ASSUMPTIONS_T),  INTENT(INOUT) :: TIME_ASSUMPTION
  LOGICAL,                   INTENT(IN)    :: VERBOSE

  ! Local variables
  CHARACTER(LEN=:), ALLOCATABLE :: CLTMP
  INTEGER(KIND=JPIB_K) :: ITMP
  LOGICAL :: LTMP
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  !
  IF ( CFG%GET( 'type-of-time-range', CLTMP  ) ) THEN
    ! Check if the field is allocated
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CLTMP), 0 )
    ! Read integer of string and check for validity
    IF ( IS_INTEGER(CLTMP) ) THEN
      READ(CLTMP,* ) ITMP
      IF ( ALL( ITMP .NE. [ TYPE_OF_TIME_RANGE_INSTANT_E,     &
&                           TYPE_OF_TIME_RANGE_FROM_STEP0_E,  &
&                           TYPE_OF_TIME_RANGE_FROM_LASTPP_E, &
&                           TYPE_OF_TIME_RANGE_FIXED_SIZE_E ] ) ) THEN
        PP_DEBUG_CRITICAL_THROW( 1 )
      ENDIF
    ELSE
      SELECT CASE( TOLOWER(CLTMP) )
      CASE ( 'instant' )
        ITMP = TYPE_OF_TIME_RANGE_INSTANT_E
      CASE ( 'from-step-zero' )
        ITMP = TYPE_OF_TIME_RANGE_FROM_STEP0_E
      CASE ( 'from-last-pp' )
        ITMP = TYPE_OF_TIME_RANGE_FROM_LASTPP_E
      CASE ( 'fixed-size' )
        ITMP = TYPE_OF_TIME_RANGE_FIXED_SIZE_E
      CASE DEFAULT
        PP_DEBUG_CRITICAL_THROW( 2 )
      END SELECT
    ENDIF
    ! Consistency checks
    IF ( TIME_ASSUMPTION%PRODUCT_DEFINITION_TEMPLATE_NUMBER .EQ. 0 .OR. &
&        TIME_ASSUMPTION%PRODUCT_DEFINITION_TEMPLATE_NUMBER .EQ. 1 ) THEN
      IF ( ITMP .EQ. TYPE_OF_TIME_RANGE_INSTANT_E ) THEN
        TIME_ASSUMPTION%TYPE_OF_TIME_RANGE = ITMP
      ELSE
        PP_DEBUG_CRITICAL_THROW( 3 )
      ENDIF
    ELSE
      TIME_ASSUMPTION%TYPE_OF_TIME_RANGE = ITMP
    ENDIF
    IF ( VERBOSE ) THEN
      WRITE(ERROR_UNIT,*) 'TypeOfTimeRange: ',  TIME_ASSUMPTION%TYPE_OF_TIME_RANGE
    ENDIF
    IF (ALLOCATED(CLTMP)) DEALLOCATE(CLTMP)
  ELSE
    IF (ALLOCATED(CLTMP)) DEALLOCATE(CLTMP)
      IF ( TIME_ASSUMPTION%PRODUCT_DEFINITION_TEMPLATE_NUMBER .EQ. 0 .OR. &
&          TIME_ASSUMPTION%PRODUCT_DEFINITION_TEMPLATE_NUMBER .EQ. 1 ) THEN
         TIME_ASSUMPTION%TYPE_OF_TIME_RANGE =  TYPE_OF_TIME_RANGE_INSTANT_E
    ELSE
      PP_DEBUG_CRITICAL_THROW( 4 )
    ENDIF
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
    CHARACTER(LEN=32) :: CPDT
    CHARACTER(LEN=1024) :: CTTR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (0)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Field not allocated' )
    CASE (1)
      CPDT = REPEAT(' ',32)
      WRITE(CPDT,*) ITMP
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Invalid value for "type-of-time-range", expected one of [1,2,3,4], got: '//TRIM(ADJUSTL(CPDT)) )
    CASE (2)
      CTTR = REPEAT(' ',1024)
      CTTR='["instant","from-step-zero","from-last-pp","fixed-size"]'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Invalid value for "type-of-time-range", expected one of '//TRIM(ADJUSTL(CTTR))//', got: '//TRIM(ADJUSTL(CLTMP)) )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, '"type-of-time-range" is inconsistent with "product-definition-template-number"' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'expected keyword "type-of-time-range" not present in the rule' )
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

END SUBROUTINE READ_TYPE_OF_TIME_RANGE_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_TYPE_OF_STATISTICAL_PROCESS_CFG'
SUBROUTINE READ_TYPE_OF_STATISTICAL_PROCESS_CFG( CFG, TIME_ASSUMPTION, VERBOSE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_INSTANT_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_AVERAGE_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_ACCUMUL_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_MIN_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_MAX_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_SEVERITY_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_MODE_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_INSTANT_E
  USE :: OM_GENERAL_UTILS_MOD, ONLY: TOLOWER

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FCKIT_CONFIGURATION), INTENT(IN)    :: CFG
  TYPE(TIME_ASSUMPTIONS_T),  INTENT(INOUT) :: TIME_ASSUMPTION
  LOGICAL,                   INTENT(IN)    :: VERBOSE

  ! Local variables
  CHARACTER(LEN=:), ALLOCATABLE :: CLTMP
  INTEGER(KIND=JPIB_K) :: ITMP
  LOGICAL :: LTMP
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( CFG%GET( 'type-of-statistical-process', CLTMP  ) ) THEN
    ! Check if the field is allocated
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CLTMP), 0 )

    ! Read integer or string
    IF ( IS_INTEGER(CLTMP) ) THEN

      READ(CLTMP,* ) ITMP
      IF ( ALL( ITMP .NE. [ TYPE_OF_STATISTICAL_PROCESS_INSTANT_E, &
&                           TYPE_OF_STATISTICAL_PROCESS_AVERAGE_E, &
&                           TYPE_OF_STATISTICAL_PROCESS_ACCUMUL_E, &
&                           TYPE_OF_STATISTICAL_PROCESS_MIN_E, &
&                           TYPE_OF_STATISTICAL_PROCESS_MAX_E, &
&                           TYPE_OF_STATISTICAL_PROCESS_SEVERITY_E, &
&                           TYPE_OF_STATISTICAL_PROCESS_MODE_E ] ) ) THEN
        PP_DEBUG_CRITICAL_THROW( 1 )
      ENDIF

    ELSE

      SELECT CASE( TOLOWER(CLTMP) )
      CASE ( 'average' )
        ITMP = TYPE_OF_STATISTICAL_PROCESS_AVERAGE_E
      CASE ( 'accumul', 'accumulation' )
        ITMP = TYPE_OF_STATISTICAL_PROCESS_ACCUMUL_E
      CASE ( 'min' )
        ITMP = TYPE_OF_STATISTICAL_PROCESS_MIN_E
      CASE ( 'max' )
        ITMP = TYPE_OF_STATISTICAL_PROCESS_MAX_E
      CASE ( 'severity' )
        ITMP = TYPE_OF_STATISTICAL_PROCESS_SEVERITY_E
      CASE ( 'mode' )
        ITMP = TYPE_OF_STATISTICAL_PROCESS_MODE_E
      CASE ( 'instant' )
        ITMP = TYPE_OF_STATISTICAL_PROCESS_INSTANT_E
      CASE DEFAULT
        PP_DEBUG_CRITICAL_THROW( 2 )
      END SELECT

    ENDIF

    ! Consistency checks
    IF ( ITMP .EQ. TYPE_OF_STATISTICAL_PROCESS_INSTANT_E ) THEN
      IF ( TIME_ASSUMPTION%TYPE_OF_TIME_RANGE .EQ. TYPE_OF_TIME_RANGE_INSTANT_E ) THEN
        TIME_ASSUMPTION%TYPE_OF_STATISTICAL_PROCESSING = ITMP
      ELSE
        PP_DEBUG_CRITICAL_THROW( 3 )
      ENDIF
    ELSE
      TIME_ASSUMPTION%TYPE_OF_STATISTICAL_PROCESSING = ITMP
    ENDIF
    IF ( VERBOSE ) THEN
      WRITE(ERROR_UNIT,*) 'TypeOfStatisticalProcessing: ',  TIME_ASSUMPTION%TYPE_OF_STATISTICAL_PROCESSING
    ENDIF
    IF (ALLOCATED(CLTMP)) DEALLOCATE(CLTMP)
  ELSE
    IF (ALLOCATED(CLTMP)) THEN
      DEALLOCATE(CLTMP)
    ENDIF
    IF ( TIME_ASSUMPTION%TYPE_OF_TIME_RANGE .EQ. TYPE_OF_TIME_RANGE_INSTANT_E ) THEN
      TIME_ASSUMPTION%TYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_INSTANT_E
    ELSE
      PP_DEBUG_CRITICAL_THROW( 4 )
    ENDIF
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
    CHARACTER(LEN=32) :: CPDT
    CHARACTER(LEN=1024) :: CTTR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (0)
      PP_DEBUG_CREATE_ERROR_MSG( STR, '"type-of-statistical-process" not allocated'  )
    CASE (1)
      CPDT = REPEAT(' ',32)
      WRITE(CPDT,*) ITMP
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Invalid value for "type-of-statistical-process", expected one of [1,2,3,4,5,6,7], got: '//TRIM(ADJUSTL(CPDT)) )
    CASE (2)
      CTTR = REPEAT(' ',1024)
      CTTR='["instant","average","accumul|accumulation","min","max","severity","mode"]'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Invalid value for "type-of-statistical-process", expected one of '//TRIM(ADJUSTL(CTTR))//', got: '//TRIM(ADJUSTL(CLTMP)) )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'If "type-of-statistical-process", then the "type-of-time-range" must be "intant"' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, '"type-of-statistical-process" kleyword expected' )
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

END SUBROUTINE READ_TYPE_OF_STATISTICAL_PROCESS_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_LENGTH_OF_TIME_RANGE_IN_SECONDS_CFG'
SUBROUTINE READ_LENGTH_OF_TIME_RANGE_IN_SECONDS_CFG( CFG, TIME_ASSUMPTION, VERBOSE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_INSTANT_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_AVERAGE_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_ACCUMUL_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_MIN_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_MAX_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_SEVERITY_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_MODE_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_INSTANT_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FROM_STEP0_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FROM_LASTPP_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FIXED_SIZE_E

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FCKIT_CONFIGURATION), INTENT(IN)    :: CFG
  TYPE(TIME_ASSUMPTIONS_T),  INTENT(INOUT) :: TIME_ASSUMPTION
  LOGICAL,                   INTENT(IN)    :: VERBOSE

  ! Local variables
  CHARACTER(LEN=:), ALLOCATABLE :: CLTMP
  INTEGER(KIND=JPIB_K) :: ITMP
  LOGICAL :: LTMP
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

IF ( CFG%GET( 'length-of-time-range-in-seconds', ITMP  ) ) THEN
    ! length must be multiple of one hour
    PP_DEBUG_CRITICAL_COND_THROW( MOD(ITMP, 3600_JPIB_K).NE.0_JPIB_K, 0 )
    ! Consistency checks
    IF ( TIME_ASSUMPTION%TYPE_OF_TIME_RANGE .EQ. TYPE_OF_TIME_RANGE_INSTANT_E ) THEN
      IF ( ITMP .NE. 0 ) THEN
        PP_DEBUG_CRITICAL_THROW( 1 )
      ELSE
        TIME_ASSUMPTION%LENGTH_OF_TIME_RANGE_IN_SECONDS = ITMP
      ENDIF
    ELSEIF ( TIME_ASSUMPTION%TYPE_OF_TIME_RANGE .EQ. TYPE_OF_TIME_RANGE_FROM_STEP0_E ) THEN
      IF ( ITMP .NE. -1 ) THEN
        PP_DEBUG_CRITICAL_THROW( 2 )
      ELSE
        TIME_ASSUMPTION%LENGTH_OF_TIME_RANGE_IN_SECONDS = ITMP
      ENDIF
    ELSEIF ( TIME_ASSUMPTION%TYPE_OF_TIME_RANGE .EQ. TYPE_OF_TIME_RANGE_FROM_LASTPP_E ) THEN
      IF ( ITMP .NE. -2 ) THEN
        PP_DEBUG_CRITICAL_THROW( 3 )
      ELSE
        TIME_ASSUMPTION%LENGTH_OF_TIME_RANGE_IN_SECONDS = ITMP
      ENDIF
    ELSEIF ( TIME_ASSUMPTION%TYPE_OF_TIME_RANGE .EQ. TYPE_OF_TIME_RANGE_FIXED_SIZE_E ) THEN
      IF ( ITMP .LE. 0 ) THEN
        PP_DEBUG_CRITICAL_THROW( 4 )
      ELSE
        TIME_ASSUMPTION%LENGTH_OF_TIME_RANGE_IN_SECONDS = ITMP
      ENDIF
    ELSE
      PP_DEBUG_CRITICAL_THROW( 5 )
    ENDIF
    IF ( VERBOSE ) THEN
      WRITE(ERROR_UNIT,*) 'LengthOfTimeRange: ',  TIME_ASSUMPTION%LENGTH_OF_TIME_RANGE_IN_SECONDS
    ENDIF
  ELSE
    SELECT CASE (TIME_ASSUMPTION%TYPE_OF_TIME_RANGE)
    CASE (TYPE_OF_TIME_RANGE_INSTANT_E)
      TIME_ASSUMPTION%LENGTH_OF_TIME_RANGE_IN_SECONDS = 0
    CASE (TYPE_OF_TIME_RANGE_FROM_STEP0_E)
      TIME_ASSUMPTION%LENGTH_OF_TIME_RANGE_IN_SECONDS = -1
    CASE (TYPE_OF_TIME_RANGE_FROM_LASTPP_E)
      TIME_ASSUMPTION%LENGTH_OF_TIME_RANGE_IN_SECONDS = -2
    CASE DEFAULT
      PP_DEBUG_CRITICAL_THROW( 6 )
    END SELECT
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
    CHARACTER(LEN=32) :: CPDT

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (0)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'length must be multiple of one hour' )
    CASE (1)
      CPDT = REPEAT(' ',32)
      WRITE(CPDT,*) ITMP
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'When "type-of-time-range" is "instant", "length-of-time-range-in-seconds" must be 0, instead is: '//TRIM(ADJUSTL(CPDT)) )
    CASE (2)
      CPDT = REPEAT(' ',32)
      WRITE(CPDT,*) ITMP
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'When "type-of-time-range" is "from-step-zero", "length-of-time-range-in-seconds" must be -1, instead is: '//TRIM(ADJUSTL(CPDT)) )
    CASE (3)
      CPDT = REPEAT(' ',32)
      WRITE(CPDT,*) ITMP
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'When "type-of-time-range" is "from-last-pp", "length-of-time-range-in-seconds" must be -2, instead is: '//TRIM(ADJUSTL(CPDT)) )
    CASE (4)
      CPDT = REPEAT(' ',32)
      WRITE(CPDT,*) ITMP
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'When "type-of-time-range" is "fixed-size", "length-of-time-range-in-seconds" must be greather than 0, instead is: '//TRIM(ADJUSTL(CPDT)) )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Invalid value for "type-of-time-range-in-seconds"' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'expected keyword "length-of-time-range-in-seconds" not present in the rule' )
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

END SUBROUTINE READ_LENGTH_OF_TIME_RANGE_IN_SECONDS_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_LENGTH_OF_TIME_RANGE_IN_HOURS_CFG'
SUBROUTINE READ_LENGTH_OF_TIME_RANGE_IN_HOURS_CFG( CFG, TIME_ASSUMPTION, VERBOSE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_INSTANT_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_AVERAGE_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_ACCUMUL_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_MIN_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_MAX_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_SEVERITY_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_MODE_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_INSTANT_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FROM_STEP0_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FROM_LASTPP_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FIXED_SIZE_E

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FCKIT_CONFIGURATION), INTENT(IN)    :: CFG
  TYPE(TIME_ASSUMPTIONS_T),  INTENT(INOUT) :: TIME_ASSUMPTION
  LOGICAL,                   INTENT(IN)    :: VERBOSE

  ! Local variables
  CHARACTER(LEN=:), ALLOCATABLE :: CLTMP
  INTEGER(KIND=JPIB_K) :: ITMP
  LOGICAL :: LTMP
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

IF ( CFG%GET( 'length-of-time-range-in-hours', ITMP  ) ) THEN
    ! Consistency checks
    IF ( TIME_ASSUMPTION%TYPE_OF_TIME_RANGE .EQ. TYPE_OF_TIME_RANGE_INSTANT_E ) THEN
      IF ( ITMP .NE. 0 ) THEN
        PP_DEBUG_CRITICAL_THROW( 1 )
      ELSE
        TIME_ASSUMPTION%LENGTH_OF_TIME_RANGE_IN_SECONDS = 0
      ENDIF
    ELSEIF ( TIME_ASSUMPTION%TYPE_OF_TIME_RANGE .EQ. TYPE_OF_TIME_RANGE_FROM_STEP0_E ) THEN
      IF ( ITMP .NE. -1 ) THEN
        PP_DEBUG_CRITICAL_THROW( 2 )
      ELSE
        TIME_ASSUMPTION%LENGTH_OF_TIME_RANGE_IN_SECONDS = -1
      ENDIF
    ELSEIF ( TIME_ASSUMPTION%TYPE_OF_TIME_RANGE .EQ. TYPE_OF_TIME_RANGE_FROM_LASTPP_E ) THEN
      IF ( ITMP .NE. -2 ) THEN
        PP_DEBUG_CRITICAL_THROW( 3 )
      ELSE
        TIME_ASSUMPTION%LENGTH_OF_TIME_RANGE_IN_SECONDS = -2
      ENDIF
    ELSEIF ( TIME_ASSUMPTION%TYPE_OF_TIME_RANGE .EQ. TYPE_OF_TIME_RANGE_FIXED_SIZE_E ) THEN
      IF ( ITMP .LE. 0 ) THEN
        PP_DEBUG_CRITICAL_THROW( 4 )
      ELSE
        TIME_ASSUMPTION%LENGTH_OF_TIME_RANGE_IN_SECONDS = ITMP*3600
      ENDIF
    ELSE
      PP_DEBUG_CRITICAL_THROW( 5 )
    ENDIF
    IF ( VERBOSE ) THEN
      WRITE(ERROR_UNIT,*) 'LengthOfTimeRange: ',  TIME_ASSUMPTION%LENGTH_OF_TIME_RANGE_IN_SECONDS
    ENDIF
  ELSE
    SELECT CASE (TIME_ASSUMPTION%TYPE_OF_TIME_RANGE)
    CASE (TYPE_OF_TIME_RANGE_INSTANT_E)
      TIME_ASSUMPTION%LENGTH_OF_TIME_RANGE_IN_SECONDS = 0
    CASE (TYPE_OF_TIME_RANGE_FROM_STEP0_E)
      TIME_ASSUMPTION%LENGTH_OF_TIME_RANGE_IN_SECONDS = -1
    CASE (TYPE_OF_TIME_RANGE_FROM_LASTPP_E)
      TIME_ASSUMPTION%LENGTH_OF_TIME_RANGE_IN_SECONDS = -2
    CASE DEFAULT
      PP_DEBUG_CRITICAL_THROW( 6 )
    END SELECT
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
    CHARACTER(LEN=32) :: CPDT

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      CPDT = REPEAT(' ',32)
      WRITE(CPDT,*) ITMP
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'When "type-of-time-range" is "instant", "length-of-time-range-in-hours" must be 0, instead is: '//TRIM(ADJUSTL(CPDT)) )
    CASE (2)
      CPDT = REPEAT(' ',32)
      WRITE(CPDT,*) ITMP
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'When "type-of-time-range" is "from-step-zero", "length-of-time-range-in-hours" must be -1, instead is: '//TRIM(ADJUSTL(CPDT)) )
    CASE (3)
      CPDT = REPEAT(' ',32)
      WRITE(CPDT,*) ITMP
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'When "type-of-time-range" is "from-last-pp", "length-of-time-range-in-hours" must be -2, instead is: '//TRIM(ADJUSTL(CPDT)) )
    CASE (4)
      CPDT = REPEAT(' ',32)
      WRITE(CPDT,*) ITMP
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'When "type-of-time-range" is "fixed-size", "length-of-time-range-in-hours" must be greather than 0, instead is: '//TRIM(ADJUSTL(CPDT)) )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Invalid value for "type-of-time-range-in-hours"' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'expected keyword "length-of-time-range-in-hours" not present in the rule' )
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

END SUBROUTINE READ_LENGTH_OF_TIME_RANGE_IN_HOURS_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_EMIT_STEP_ZERO_CFG'
SUBROUTINE READ_EMIT_STEP_ZERO_CFG( CFG, TIME_ASSUMPTION, VERBOSE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_INSTANT_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_ACCUMUL_E

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FCKIT_CONFIGURATION), INTENT(IN)    :: CFG
  TYPE(TIME_ASSUMPTIONS_T),  INTENT(INOUT) :: TIME_ASSUMPTION
  LOGICAL,                   INTENT(IN)    :: VERBOSE

  ! Local variables
  CHARACTER(LEN=:), ALLOCATABLE :: CLTMP
  INTEGER(KIND=JPIB_K) :: ITMP
  LOGICAL :: LTMP
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( CFG%GET( 'emit-step-zero', LTMP  ) ) THEN
    IF (TIME_ASSUMPTION%TYPE_OF_STATISTICAL_PROCESSING .EQ. TYPE_OF_STATISTICAL_PROCESS_ACCUMUL_E ) THEN
      TIME_ASSUMPTION%EMIT_STEP_ZERO = LTMP
    ELSEIF (TIME_ASSUMPTION%TYPE_OF_STATISTICAL_PROCESSING .EQ. TYPE_OF_STATISTICAL_PROCESS_INSTANT_E ) THEN
      IF ( LTMP) THEN
        TIME_ASSUMPTION%EMIT_STEP_ZERO = LTMP
      ELSE
        PP_DEBUG_CRITICAL_THROW( 1 )
      ENDIF
    ELSE
      IF ( .NOT. LTMP) THEN
        TIME_ASSUMPTION%EMIT_STEP_ZERO = LTMP
      ELSE
        PP_DEBUG_CRITICAL_THROW( 2 )
      ENDIF
    ENDIF
    IF ( VERBOSE ) THEN
      WRITE(ERROR_UNIT,*) 'EmitStepZero: ',  TIME_ASSUMPTION%EMIT_STEP_ZERO
    ENDIF
  ELSE
    IF (TIME_ASSUMPTION%TYPE_OF_STATISTICAL_PROCESSING .EQ. TYPE_OF_STATISTICAL_PROCESS_INSTANT_E ) THEN
      TIME_ASSUMPTION%EMIT_STEP_ZERO = .TRUE.
    ELSE
      IF ( TIME_ASSUMPTION%TYPE_OF_STATISTICAL_PROCESSING .NE. TYPE_OF_STATISTICAL_PROCESS_ACCUMUL_E ) THEN
        TIME_ASSUMPTION%EMIT_STEP_ZERO = .FALSE.
      ELSE
        PP_DEBUG_CRITICAL_THROW( 3 )
      ENDIF
    ENDIF
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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Always emit step zero for instant fields' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Never emit step zero for statistical fileds with operation type different from accumulation' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, '"emit-step-zero" expected' )
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

END SUBROUTINE READ_EMIT_STEP_ZERO_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'CREPRES2IREPRES'
FUNCTION CREPRES2IREPRES( CREPRES ) RESULT(IREPRES)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,   ONLY: REPRES_GRIDDED_E
  USE :: OM_CORE_MOD,   ONLY: REPRES_SPECTRAL_E
  USE :: OM_CORE_MOD,   ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*), INTENT(IN) :: CREPRES

  ! Function result
  INTEGER(KIND=JPIB_K) :: IREPRES

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  SELECT CASE ( CREPRES )

  CASE ( 'gg', 'gridded' )
    IREPRES = REPRES_GRIDDED_E
  CASE ( 'sh', 'spherical_harmonics' )
    IREPRES = REPRES_SPECTRAL_E
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

END FUNCTION CREPRES2IREPRES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'IS_INTEGER'
FUNCTION IS_INTEGER(STR) RESULT(ISINT)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*), INTENT(IN) :: STR

  ! Function result
  LOGICAL :: ISINT

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: LENGTH

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  LENGTH = LEN_TRIM(STR)

  ! Initialize ISINT to false
  ISINT = .FALSE.

  ! Check each character to see if it's a digit
  DO I = 1, LENGTH
    IF (.NOT.( ICHAR(STR(I:I)) .GE. ICHAR('0') .AND. &
&              ICHAR(STR(I:I)) .LE. ICHAR('9')) ) THEN
      RETURN
    END IF
  END DO

  ! If all characters are digits, set ISINT to true
  ISINT = .TRUE.

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION IS_INTEGER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'IS_INTEGER_RANGE'
FUNCTION IS_INTEGER_RANGE(STR) RESULT(ISINT)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*), INTENT(IN) :: STR

  ! Function result
  LOGICAL :: ISINT

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: LENGTH

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  LENGTH = LEN_TRIM(STR)

  ! Initialize ISINT to false
  ISINT = .FALSE.

  ! Check each character to see if it's a digit
  CNT = 0
  DO I = 1, LENGTH
    IF (.NOT.( ICHAR(STR(I:I)) .GE. ICHAR('0') .AND. &
&              ICHAR(STR(I:I)) .LE. ICHAR('9')) ) THEN
      IF ( I.GT.1 .AND. I.LT.LENGTH .AND. CNT.EQ.0 .AND. STR(I:I).EQ.':' ) THEN
        CNT = CNT + 1
      ELSE
        RETURN
      ENDIF
    END IF
  END DO

  ! If all characters are digits, set ISINT to true
  IF ( CNT .EQ.1 ) THEN
    ISINT = .TRUE.
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION IS_INTEGER_RANGE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_INTEGER_RANGE'
SUBROUTINE READ_INTEGER_RANGE(STR, LO, HI)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)  :: STR
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: LO
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: HI

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: IDX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  PP_DEBUG_CRITICAL_COND_THROW( .NOT.IS_INTEGER_RANGE(STR), 1 )

  IDX = INDEX(STR,':')

  READ(STR(1:IDX-1),*)  LO
  READ(STR(IDX+1:),*)  HI

  PP_DEBUG_CRITICAL_COND_THROW( LO.GE.HI, 2 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'String is not an integer range' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'lower bound needs to be lower that the upper bound' )
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

END SUBROUTINE READ_INTEGER_RANGE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MATCH_ASSUMPTIONS_RULES'
SUBROUTINE MATCH_ASSUMPTIONS_RULES( PARAM_ID, LEV_TYPE, REPRES, LEVEL, IS_ENSAMBLE, TIME_ASSUMPTIONS, LEVEL_ASSUMPTIONS )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,   ONLY: JPIB_K
  USE :: OM_CORE_MOD,   ONLY: UNDEF_PARAM_E
  USE :: MSG_UTILS_MOD, ONLY: ILEVTYPE2CLEVTYPE
  USE :: MSG_UTILS_MOD, ONLY: IREPRES2CREPRES

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),      INTENT(IN)  :: PARAM_ID
  INTEGER(KIND=JPIB_K),      INTENT(IN)  :: LEV_TYPE
  INTEGER(KIND=JPIB_K),      INTENT(IN)  :: REPRES
  INTEGER(KIND=JPIB_K),      INTENT(IN)  :: LEVEL
  LOGICAL,                   INTENT(IN)  :: IS_ENSAMBLE
  TYPE(TIME_ASSUMPTIONS_T),  INTENT(OUT) :: TIME_ASSUMPTIONS
  TYPE(LEVEL_ASSUMPTIONS_T), INTENT(OUT) :: LEVEL_ASSUMPTIONS

  ! Local variables
  CHARACTER(LEN=16)    :: CI
  CHARACTER(LEN=16)    :: CPARAM_ID
  CHARACTER(LEN=16)    :: CLEVEL
  CHARACTER(LEN=16)    :: CENS
  CHARACTER(LEN=4096)  :: RMATCH
  CHARACTER(LEN=1024)  :: FLDSTR
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: RID
  LOGICAL :: LTMP
  LOGICAL :: MATCH

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Local variables initialisation
  CNT = 0
  RID = 0

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(SPECIAL_ASSUMPTIONS), 1 )

  ! String for the input field to be parsed
  FLDSTR = REPEAT(' ',1024)
  WRITE(CPARAM_ID,'(I16)') PARAM_ID
  WRITE(CLEVEL,'(I16)') LEVEL
  WRITE(CENS,'(L4)') IS_ENSAMBLE
  WRITE(FLDSTR,'(A)') &
&          '{ paramID='//TRIM(ADJUSTL(CPARAM_ID))// &
&          ', levtype="'//TRIM(ADJUSTL(ILEVTYPE2CLEVTYPE(LEV_TYPE)))// &
&          '", repres="'//TRIM(ADJUSTL(IREPRES2CREPRES(REPRES)))// &
&          '", level='//TRIM(ADJUSTL(CLEVEL))// &
&          ', is-ensamble='//TRIM(ADJUSTL(CENS))//'}'

  ! Search loop
  RMATCH = '( '
  DO I = 1, SIZE(SPECIAL_ASSUMPTIONS)

    ! Initialize the match variable
    MATCH = ANY( [ ALLOCATED(SPECIAL_ASSUMPTIONS(I)%MATCHER%LEV_TYPE), &
&                  ALLOCATED(SPECIAL_ASSUMPTIONS(I)%MATCHER%REPRES),   &
&                  ALLOCATED(SPECIAL_ASSUMPTIONS(I)%MATCHER%LEVEL),    &
&                  ALLOCATED(SPECIAL_ASSUMPTIONS(I)%MATCHER%PARAM_ID), &
&                   SPECIAL_ASSUMPTIONS(I)%MATCHER%IS_ENSAMBLE .EQV. IS_ENSAMBLE ] )

    IF ( ALLOCATED(SPECIAL_ASSUMPTIONS(I)%MATCHER%LEV_TYPE) ) THEN
      LTMP = ANY( SPECIAL_ASSUMPTIONS(I)%MATCHER%LEV_TYPE .EQ. LEV_TYPE )
      MATCH = MATCH .AND. LTMP
    ENDIF

    IF ( ALLOCATED(SPECIAL_ASSUMPTIONS(I)%MATCHER%REPRES) ) THEN
      LTMP = ANY( SPECIAL_ASSUMPTIONS(I)%MATCHER%REPRES .EQ. REPRES )
      MATCH = MATCH .AND. LTMP
    ENDIF

    IF ( ALLOCATED(SPECIAL_ASSUMPTIONS(I)%MATCHER%LEVEL) ) THEN
      LTMP = ANY( SPECIAL_ASSUMPTIONS(I)%MATCHER%LEVEL .EQ. LEVEL )
      MATCH = MATCH .AND. LTMP
    ENDIF

    IF ( ALLOCATED(SPECIAL_ASSUMPTIONS(I)%MATCHER%PARAM_ID) ) THEN
      LTMP = ANY( SPECIAL_ASSUMPTIONS(I)%MATCHER%PARAM_ID .EQ. PARAM_ID )
      MATCH = MATCH .AND. LTMP
    ENDIF

    LTMP = SPECIAL_ASSUMPTIONS(I)%MATCHER%IS_ENSAMBLE .EQV. IS_ENSAMBLE
    MATCH = MATCH .AND. LTMP

    ! Update match counter
    IF ( MATCH ) THEN
      CNT = CNT + 1
      RID = I
      WRITE(CI,'(I10)') I
      IF ( CNT .EQ. 1 ) THEN
        RMATCH = TRIM(RMATCH)//'{type="special", id='//TRIM(ADJUSTL(CI))//', name="'//TRIM(ADJUSTL(SPECIAL_ASSUMPTIONS(I)%NAME))//'"}'
      ELSE
        RMATCH = TRIM(RMATCH)//', '//'{type="special", id='//TRIM(ADJUSTL(CI))//', name="'//TRIM(ADJUSTL(SPECIAL_ASSUMPTIONS(I)%NAME))//'"}'
      ENDIF
    ENDIF

  ENDDO

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( CNT.GT.1, 2 )

  ! Apply default rule if needed
  IF ( CNT.LT.1 ) THEN

    ! Error handling
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(DEFAULT_ASSUMPTIONS), 3 )

    ! Local variables initialisation
    CNT = 0
    RID = 0

    ! Search loop
    RMATCH = '( '
    DO I = 1, SIZE(DEFAULT_ASSUMPTIONS)

      ! Initialize the match variable
      MATCH = ANY( [ ALLOCATED(DEFAULT_ASSUMPTIONS(I)%MATCHER%LEV_TYPE), &
  &                  ALLOCATED(DEFAULT_ASSUMPTIONS(I)%MATCHER%REPRES),   &
  &                  ALLOCATED(DEFAULT_ASSUMPTIONS(I)%MATCHER%LEVEL),    &
  &                  ALLOCATED(DEFAULT_ASSUMPTIONS(I)%MATCHER%PARAM_ID), &
  &                   DEFAULT_ASSUMPTIONS(I)%MATCHER%IS_ENSAMBLE .EQV. IS_ENSAMBLE ] )

      IF ( ALLOCATED(DEFAULT_ASSUMPTIONS(I)%MATCHER%LEV_TYPE) ) THEN
        LTMP = ANY( DEFAULT_ASSUMPTIONS(I)%MATCHER%LEV_TYPE .EQ. LEV_TYPE )
        MATCH = MATCH .AND. LTMP
      ENDIF

      IF ( ALLOCATED(DEFAULT_ASSUMPTIONS(I)%MATCHER%REPRES) ) THEN
        LTMP = ANY( DEFAULT_ASSUMPTIONS(I)%MATCHER%REPRES .EQ. REPRES )
        MATCH = MATCH .AND. LTMP
      ENDIF

      IF ( ALLOCATED(DEFAULT_ASSUMPTIONS(I)%MATCHER%LEVEL) ) THEN
        LTMP = ANY( DEFAULT_ASSUMPTIONS(I)%MATCHER%LEVEL .EQ. LEVEL )
        MATCH = MATCH .AND. LTMP
      ENDIF

      IF ( ALLOCATED(DEFAULT_ASSUMPTIONS(I)%MATCHER%PARAM_ID) ) THEN
        LTMP = ANY( DEFAULT_ASSUMPTIONS(I)%MATCHER%PARAM_ID .EQ. PARAM_ID )
        MATCH = MATCH .AND. LTMP
      ENDIF

      LTMP = DEFAULT_ASSUMPTIONS(I)%MATCHER%IS_ENSAMBLE .EQV. IS_ENSAMBLE
      MATCH = MATCH .AND. LTMP

      ! Update match counter
      IF ( MATCH ) THEN
        CNT = CNT + 1
        RID = I
        WRITE(CI,'(I10)') I
        IF ( CNT .EQ. 1 ) THEN
          RMATCH = TRIM(RMATCH)//'{type="default", id='//TRIM(ADJUSTL(CI))//', name="'//TRIM(ADJUSTL(DEFAULT_ASSUMPTIONS(I)%NAME))//'"}'
        ELSE
          RMATCH = TRIM(RMATCH)//', '//'{type="default", id='//TRIM(ADJUSTL(CI))//', name="'//TRIM(ADJUSTL(DEFAULT_ASSUMPTIONS(I)%NAME))//'"}'
        ENDIF
      ENDIF

    ENDDO

    ! Error handling
    PP_DEBUG_CRITICAL_COND_THROW( CNT.GT.1, 4 )

    IF ( CNT.LT.1 ) THEN
      PP_DEBUG_CRITICAL_THROW( 5 )
    ELSE
      WRITE(*,*) 'ACCESSING RID: ', RID, SIZE(DEFAULT_ASSUMPTIONS)
      TIME_ASSUMPTIONS = DEFAULT_ASSUMPTIONS(RID)%TIME_ASSUMPTIONS
      LEVEL_ASSUMPTIONS = DEFAULT_ASSUMPTIONS(RID)%LEVEL_ASSUMPTIONS
    ENDIF
  ELSE
    ! Associate the rule definitions to the output variable
    TIME_ASSUMPTIONS  = SPECIAL_ASSUMPTIONS(RID)%TIME_ASSUMPTIONS
    LEVEL_ASSUMPTIONS = SPECIAL_ASSUMPTIONS(RID)%LEVEL_ASSUMPTIONS
  ENDIF


  ! Loggin rules match
  IF ( CNT  .EQ. 1 ) THEN
    WRITE(*,*) ' + ASSUMPTIONS_RULES_LOG: applied rule: '//TRIM(ADJUSTL(RMATCH))//') to field: '//TRIM(ADJUSTL(FLDSTR))
  ELSE
    WRITE(*,*) ' + ASSUMPTIONS_RULES_LOG: applied default rule to field: '//TRIM(ADJUSTL(FLDSTR))
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
    CHARACTER(LEN=1024) :: TMP2

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Rules not allocated' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'No match found for field: '//TRIM(ADJUSTL(FLDSTR)) )
    CASE (3)
      RMATCH = TRIM(RMATCH)//' )'
      WRITE(TMP2,'(I8)') CNT
      PP_DEBUG_CREATE_ERROR_MSG( STR, TRIM(ADJUSTL(TMP2))//' matches rules=>'//TRIM(ADJUSTL(RMATCH))//' found for field: '//TRIM(ADJUSTL(FLDSTR)) )
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


END SUBROUTINE MATCH_ASSUMPTIONS_RULES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE YAML_TIME_ASSUMPTIONS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
