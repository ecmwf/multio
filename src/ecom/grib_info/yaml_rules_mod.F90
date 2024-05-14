! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'rules_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'YAML_RULES_MOD'
MODULE YAML_RULES_MOD

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

IMPLICIT NONE

! Default visibility of the module
PRIVATE


!> Rules used to match fields
TYPE :: MATCHER_T
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE :: LEV_TYPE
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE :: LEVEL
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE :: REPRES
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE :: PARAM_ID
END TYPE


!> Configurations loaded from yaml
TYPE :: DEFINITIONS_T
  INTEGER(KIND=JPIB_K) :: EDITION=-99
  INTEGER(KIND=JPIB_K) :: BITS_PER_VALUE=-99
  INTEGER(KIND=JPIB_K) :: PACKING_TYPE=-99
END TYPE

!> Container for rules
TYPE :: RULE_T
  CHARACTER(LEN=128)  :: NAME
  TYPE(MATCHER_T)     :: MATCHER
  TYPE(DEFINITIONS_T) :: DEFINITIONS
END TYPE

!> Set of all encoding rules to be used
TYPE(RULE_T), TARGET, DIMENSION(:), ALLOCATABLE :: RULES

!> MAnagement of paramIDs
INTEGER(KIND=JPIB_K) :: MIN_PARAM_ID=-99
INTEGER(KIND=JPIB_K) :: MAX_PARAM_ID=-99
INTEGER(KIND=JPIB_K) :: NUM_PARAM_ID=-99

!> Whitelise of public symbols
PUBLIC :: DEFINITIONS_T
PUBLIC :: INIT_RULES
PUBLIC :: FREE_RULES
PUBLIC :: MATCH_RULES
PUBLIC :: RULES_DIMS

CONTAINS

#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'INIT'
SUBROUTINE INIT_RULES( CFG, VERBOSE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,  ONLY: JPIB_K
  USE :: MAP_MOD,      ONLY: MAP_T
  USE :: MAP_MOD,      ONLY: KEY_T
  USE :: MAP_MOD,      ONLY: MAP_INIT
  USE :: MAP_MOD,      ONLY: MAP_INSERT
  USE :: MAP_MOD,      ONLY: MAP_MINIMUM
  USE :: MAP_MOD,      ONLY: MAP_MAXIMUM
  USE :: MAP_MOD,      ONLY: MAP_FREE
  USE :: MAP_MOD,      ONLY: MAP_PRINT
  USE :: MAP_MOD,      ONLY: MAP_GET_SORTED_KEYS_INT
  USE :: OM_CORE_MOD,  ONLY: UNDEF_PARAM_E
  USE :: OM_CORE_MOD,  ONLY: LOOKUP_TABLE
  USE :: OM_CORE_MOD,  ONLY: LOOKUP_TABLE_BWD

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: DEALLOCATE_FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FCKIT_CONFIGURATION), INTENT(IN) :: CFG
  LOGICAL,                   INTENT(IN) :: VERBOSE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  TYPE(FCKIT_CONFIGURATION), ALLOCATABLE, DIMENSION(:) :: RULES_CFG
  TYPE(MAP_T) :: MAP
  TYPE(KEY_T) :: KEY
  CLASS(*), POINTER :: VALUE
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Reading the encoding rules
  IF ( CFG%GET( 'encoding-rules', RULES_CFG ) ) THEN

    ! Logging
    IF ( VERBOSE ) THEN
      WRITE(ERROR_UNIT,*) 'The size of the allocated object is: ', SIZE(RULES_CFG)
    ENDIF

    ! Memory allocation
    ALLOCATE(RULES(SIZE(RULES_CFG)), STAT=STAT, ERRMSG=ERRMSG )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )

    ! Loop over the rules and read rule configuration
    DO I = 1, SIZE(RULES_CFG)
      ! Logging
      IF ( VERBOSE ) THEN
        WRITE(ERROR_UNIT,*) 'Reading the next rule: ', I
      ENDIF
      CALL READ_NAME( RULES_CFG(I), RULES(I)%NAME, VERBOSE )
      CALL READ_ACTIONS( RULES_CFG(I), RULES(I), VERBOSE )
    ENDDO

    ! Log rules names
    IF ( VERBOSE ) THEN
      WRITE(ERROR_UNIT,*) RULES(1)%NAME
    ENDIF

    ! Free configuration array
    CALL DEALLOCATE_FCKIT_CONFIGURATION( RULES_CFG )

  ENDIF

  ! Compute the number of paramIds for the hash table
  CALL MAP_INIT( MAP )
  VALUE => NULL()
  IF ( ALLOCATED(RULES) ) THEN
    DO I = 1, SIZE(RULES)
      IF ( ALLOCATED(RULES(I)%MATCHER%PARAM_ID) ) THEN
        DO J = 1, SIZE(RULES(I)%MATCHER%PARAM_ID)
          KEY%K =  RULES(I)%MATCHER%PARAM_ID(J)
          VALUE => RULES(I)%MATCHER%PARAM_ID(J)
          CALL MAP_INSERT( MAP, KEY, VALUE )
        ENDDO
      ENDIF
    ENDDO
  ENDIF

  ! Extract information about paramIds
  CALL MAP_MAXIMUM( MAP, KEY, VALUE )
  MAX_PARAM_ID=KEY%K
  CALL MAP_MINIMUM( MAP, KEY, VALUE )
  MIN_PARAM_ID=KEY%K
  NUM_PARAM_ID=MAP%SIZE

  ! Allocate the buffers
  ALLOCATE( LOOKUP_TABLE( MAX_PARAM_ID ), STAT=STAT, ERRMSG=ERRMSG  )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )
  ALLOCATE( LOOKUP_TABLE_BWD( NUM_PARAM_ID ), STAT=STAT, ERRMSG=ERRMSG  )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 3 )

  ! Fill reverse lookup table
  EX = MAP_GET_SORTED_KEYS_INT( MAP, LOOKUP_TABLE_BWD )
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.EX, 4 )

  ! Construct the lookup table
  LOOKUP_TABLE = UNDEF_PARAM_E
  DO I = 1, SIZE(LOOKUP_TABLE_BWD)
    LOOKUP_TABLE(LOOKUP_TABLE_BWD(I)) = I
  ENDDO

  WRITE(*,*) LOOKUP_TABLE_BWD

  WRITE(*,*) ' + MIN: ', MIN_PARAM_ID
  WRITE(*,*) ' + MAX: ', MAX_PARAM_ID
  WRITE(*,*) ' + NUM: ', NUM_PARAM_ID

  CALL MAP_PRINT( MAP, 'test', 0_JPIB_K )

  ! Free the map
  CALL MAP_FREE( MAP )

  WRITE(*,*) 'DEBUG CODE:: ', ALLOCATED(RULES)

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
    CASE (1)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate rules: "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate rules' )
      ENDIF
    CASE (2)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate lookup_table: "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate lookup_table' )
      ENDIF
    CASE (3)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate lookup_table_bwd: "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate lookup_table_bwd' )
      ENDIF
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to extract the list of paramIds' )
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

END SUBROUTINE INIT_RULES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'FREE_RULES'
SUBROUTINE FREE_RULES()

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

  IF ( ALLOCATED(RULES) ) THEN

    ! Deallocate rules components
    DO I = 1, SIZE(RULES)
      IF ( ALLOCATED(RULES(I)%MATCHER%LEV_TYPE) ) THEN
        DEALLOCATE(RULES(I)%MATCHER%LEV_TYPE, STAT=STAT, ERRMSG=ERRMSG)
        PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )
      ENDIF
      IF ( ALLOCATED(RULES(I)%MATCHER%LEVEL) ) THEN
        DEALLOCATE(RULES(I)%MATCHER%LEVEL, STAT=STAT, ERRMSG=ERRMSG)
        PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )
      ENDIF
      IF ( ALLOCATED(RULES(I)%MATCHER%REPRES) ) THEN
        DEALLOCATE(RULES(I)%MATCHER%REPRES, STAT=STAT, ERRMSG=ERRMSG)
        PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 3 )
      ENDIF
      IF ( ALLOCATED(RULES(I)%MATCHER%PARAM_ID) ) THEN
        DEALLOCATE(RULES(I)%MATCHER%PARAM_ID, STAT=STAT, ERRMSG=ERRMSG)
        PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 4 )
      ENDIF
    ENDDO

    ! deallocate rules array
    DEALLOCATE( RULES, STAT=STAT, ERRMSG=ERRMSG)
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


END SUBROUTINE FREE_RULES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'RULES_DIMS'
SUBROUTINE RULES_DIMS( MIN, MAX, NUM )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Local variables
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: MIN
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: MAX
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: NUM

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Extract data
  MIN = MIN_PARAM_ID
  MAX = MAX_PARAM_ID
  NUM = NUM_PARAM_ID

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE RULES_DIMS
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
        WRITE(ERROR_UNIT,*) 'Rule name: ', NAME
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
  TYPE(FCKIT_CONFIGURATION) :: ENCODE_CFG

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
  IF ( CFG%GET( 'encode', ENCODE_CFG ) ) THEN

    ! Logging
    IF ( VERBOSE ) THEN
      WRITE(ERROR_UNIT,*) 'Read encode'
    ENDIF

    ! Read encdoing options
    CALL READ_ENCODE_OPTIONS( ENCODE_CFG, RULE%DEFINITIONS, VERBOSE )

    ! Deallocate YAML encoding options
    CALL ENCODE_CFG%FINAL()

  ELSE

    PP_DEBUG_CRITICAL_THROW( 2 )

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
  INTEGER(KIND=JPIB_K) :: I
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


  ! Read rules for levType
  IF ( CFG%GET( 'levType', ATMP  ) ) THEN

    IF ( ALLOCATED(ATMP) ) THEN

      ! Allocate levType rule
      ALLOCATE(MATCHER%LEV_TYPE(SIZE(ATMP)), STAT=STAT, ERRMSG=ERRMSG )
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )

      ! Convert levtype to integer and store it in the rule
      DO I = 1, SIZE(ATMP)
        MATCHER%LEV_TYPE(I) = CLEVTYPE2ILEVTYPE( ATMP(I) )
        IF ( VERBOSE ) THEN
          WRITE(ERROR_UNIT,*) 'levType: ', I, MATCHER%LEV_TYPE(I), ATMP(I), SIZE(MATCHER%LEV_TYPE)
        ENDIF
      ENDDO

      ! Free the temporary string array
      DEALLOCATE(ATMP, STAT=STAT, ERRMSG=ERRMSG )
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )

    ELSE
      PP_DEBUG_CRITICAL_THROW( 3 )
    ENDIF

  ELSEIF ( CFG%GET( 'levType', CLTMP  ) ) THEN

    IF ( ALLOCATED(ATMP) ) THEN

      ! Allocate levType rule
      ALLOCATE(MATCHER%LEV_TYPE(1), STAT=STAT, ERRMSG=ERRMSG )
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )

      ! Convert levtype to integer and store it in the rule
      MATCHER%LEV_TYPE(1) = CLEVTYPE2ILEVTYPE( CLTMP )
      IF ( VERBOSE ) THEN
        WRITE(ERROR_UNIT,*) 'levType: ', MATCHER%LEV_TYPE(1), CLTMP, SIZE(MATCHER%LEV_TYPE)
      ENDIF

      ! Free the temporary string array
      DEALLOCATE(CLTMP, STAT=STAT, ERRMSG=ERRMSG )
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

  ELSEIF ( CFG%GET( 'repres', CLTMP  ) ) THEN

    IF ( ALLOCATED(CLTMP) ) THEN

      ! Allocate repres rule
      ALLOCATE(MATCHER%REPRES(1), STAT=STAT, ERRMSG=ERRMSG )
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 4)

      ! Convert repres to integer and store it in the rule
      MATCHER%REPRES(1) = CREPRES2IREPRES( CLTMP )
      IF ( VERBOSE ) THEN
        WRITE(ERROR_UNIT,*) 'repres: ', MATCHER%REPRES(1), CLTMP, SIZE(MATCHER%REPRES)
      ENDIF

      ! Free the temporary string array
      DEALLOCATE(CLTMP, STAT=STAT, ERRMSG=ERRMSG )
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

  ELSEIF ( CFG%GET( 'level', CLTMP  ) ) THEN

    IF ( ALLOCATED(CLTMP) ) THEN

      ! Allocate level rule
      ALLOCATE( MATCHER%LEVEL(1), STAT=STAT, ERRMSG=ERRMSG )
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 7 )

      ! Store level in the rule
        PP_DEBUG_CRITICAL_COND_THROW( .NOT.IS_INTEGER( CLTMP ), 8 )
        READ(CLTMP,*) ITMP
        MATCHER%LEVEL(1) = ITMP
        IF ( VERBOSE ) THEN
          WRITE(ERROR_UNIT,*) 'level: ', MATCHER%LEVEL(1), CLTMP, SIZE(MATCHER%LEVEL)
        ENDIF

      ! Free the temporary string array
      DEALLOCATE(CLTMP, STAT=STAT, ERRMSG=ERRMSG )
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 9 )

    ELSE
      PP_DEBUG_CRITICAL_THROW( 10 )
    ENDIF

  ENDIF


  ! REad rules for paramId
  IF ( CFG%GET( 'paramId', ATMP  ) ) THEN

    IF ( ALLOCATED(ATMP) ) THEN

      ! Allocate level rule
      ALLOCATE( MATCHER%PARAM_ID(SIZE(ATMP)), STAT=STAT, ERRMSG=ERRMSG )
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 11 )

      ! Store paramId in the rule
      DO I = 1, SIZE(ATMP)
        PP_DEBUG_CRITICAL_COND_THROW( .NOT.IS_INTEGER( ATMP(I) ), 12 )
        READ(ATMP(I),*) ITMP
        MATCHER%PARAM_ID(I) = ITMP
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
#define PP_PROCEDURE_NAME 'READ_ENCODE_OPTIONS'
SUBROUTINE READ_ENCODE_OPTIONS( CFG, ENCODE_OPTIONS, VERBOSE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: PACKING_TYPE_GRIB_SIMPLE_E
  USE :: OM_CORE_MOD, ONLY: PACKING_TYPE_GRIB_CCSDE_E
  USE :: OM_CORE_MOD, ONLY: PACKING_TYPE_GRIB_COMPLEX_E
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
  TYPE(DEFINITIONS_T),       INTENT(OUT) :: ENCODE_OPTIONS
  LOGICAL,                   INTENT(IN)  :: VERBOSE

  ! Local variables
  CHARACTER(LEN=:), ALLOCATABLE :: CLTMP
  INTEGER(KIND=JPIB_K) :: ITMP
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()


  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) 'Read edition'
  ENDIF
  IF ( CFG%GET( 'gribEdition', ITMP  ) ) THEN
    ENCODE_OPTIONS%EDITION = ITMP
    IF ( VERBOSE ) THEN
      WRITE(ERROR_UNIT,*) 'Edition: ', ENCODE_OPTIONS%EDITION
    ENDIF
  ELSE
    PP_DEBUG_CRITICAL_THROW( 10 )
  ENDIF


  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) 'Read bitsPerValue'
  ENDIF
  IF ( CFG%GET( 'bitsPerValue', CLTMP  ) ) THEN
    WRITE(ERROR_UNIT,*) 'Read bitsPerValue', TRIM(CLTMP)
    IF ( IS_INTEGER(CLTMP) ) THEN
      READ(CLTMP,*) ITMP
      PP_DEBUG_CRITICAL_COND_THROW( ITMP.LT.1,  1 )
      PP_DEBUG_CRITICAL_COND_THROW( ITMP.GT.64, 2 )
      ENCODE_OPTIONS%BITS_PER_VALUE = ITMP
    ELSE
      SELECT CASE (CLTMP)
      CASE ( 'use-default-table' )
        ENCODE_OPTIONS%BITS_PER_VALUE = -1
      CASE ( 'use-compressed-table' )
        ENCODE_OPTIONS%BITS_PER_VALUE = -2
      CASE DEFAULT
        PP_DEBUG_CRITICAL_THROW( 3 )
      END SELECT
    ENDIF
    IF ( VERBOSE ) THEN
      WRITE(ERROR_UNIT,*) 'Bits per value: ', ENCODE_OPTIONS%BITS_PER_VALUE
    ENDIF
  ELSE
    ENCODE_OPTIONS%BITS_PER_VALUE = -1
  ENDIF


  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) 'Read packingType'
  ENDIF
  IF ( CFG%GET( 'packingType', CLTMP  ) ) THEN
    SELECT CASE (CLTMP)
    CASE ('grid_simple')
      ENCODE_OPTIONS%PACKING_TYPE = PACKING_TYPE_GRIB_SIMPLE_E
    CASE ('spectral_complex')
      ENCODE_OPTIONS%PACKING_TYPE = PACKING_TYPE_GRIB_COMPLEX_E
    CASE ('grid_ccsds')
      ENCODE_OPTIONS%PACKING_TYPE = PACKING_TYPE_GRIB_CCSDE_E
    CASE DEFAULT
      PP_DEBUG_CRITICAL_THROW( 4 )
    END SELECT
    IF ( VERBOSE ) THEN
      WRITE(ERROR_UNIT,*) 'Packing type: ', ENCODE_OPTIONS%PACKING_TYPE
    ENDIF
    DEALLOCATE(CLTMP)
  ELSE
    ENCODE_OPTIONS%PACKING_TYPE = UNDEF_PARAM_E
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
      PP_DEBUG_CREATE_ERROR_MSG( STR, '"bitsPerValue" lower than 1' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, '"bitsPerValue" greater than 64' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown "bitsPerValue" configuration' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown "packingType" configuration' )
    CASE (10)
      PP_DEBUG_CREATE_ERROR_MSG( STR, '"gribEdition" key non present in YAML configuration file' )
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

END SUBROUTINE READ_ENCODE_OPTIONS
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
#define PP_PROCEDURE_NAME 'MATCH_RULES'
SUBROUTINE MATCH_RULES( PARAM_ID, LEV_TYPE, REPRES, LEVEL, DEFINITIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,   ONLY: JPIB_K
  USE :: MSG_UTILS_MOD, ONLY: ILEVTYPE2CLEVTYPE
  USE :: MSG_UTILS_MOD, ONLY: IREPRES2CREPRES

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: PARAM_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: LEV_TYPE
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: REPRES
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: LEVEL
  TYPE(DEFINITIONS_T),  INTENT(OUT) :: DEFINITIONS

  ! Local variables
  CHARACTER(LEN=16)    :: CI
  CHARACTER(LEN=4096)  :: RMATCH
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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(RULES), 1 )

  ! Search loop
  RMATCH = '( '
  DO I = 1, SIZE(RULES)
    !WRITE(*,*) ' '
    !WRITE(*,*) ' CHECKING RULE(',I,'): ', RULES(I)%NAME

    ! Initialize the mathc variable
    MATCH = ANY( [ ALLOCATED(RULES(I)%MATCHER%LEV_TYPE), &
&                  ALLOCATED(RULES(I)%MATCHER%REPRES),   &
&                  ALLOCATED(RULES(I)%MATCHER%LEVEL),    &
&                  ALLOCATED(RULES(I)%MATCHER%PARAM_ID) ] )

    IF ( ALLOCATED(RULES(I)%MATCHER%LEV_TYPE) ) THEN
      LTMP = ANY( RULES(I)%MATCHER%LEV_TYPE .EQ. LEV_TYPE )
      MATCH = MATCH .AND. LTMP
    !  WRITE(*,*) 'LevType matched: ', LTMP, LEV_TYPE
    !ELSE
    !  WRITE(*,*) 'LevType automatically matched: ', LEV_TYPE
    ENDIF

    IF ( ALLOCATED(RULES(I)%MATCHER%REPRES) ) THEN
      LTMP = ANY( RULES(I)%MATCHER%REPRES .EQ. REPRES )
      MATCH = MATCH .AND. LTMP
    !  WRITE(*,*) 'Repres matched: ', LTMP, REPRES
    !ELSE
    !  WRITE(*,*) 'Repres automatically matched: ', REPRES
    ENDIF

    IF ( ALLOCATED(RULES(I)%MATCHER%LEVEL) ) THEN
      LTMP = ANY( RULES(I)%MATCHER%LEVEL .EQ. LEVEL )
      MATCH = MATCH .AND. LTMP
    !  WRITE(*,*) 'Level matched: ', LTMP, LEVEL
    !ELSE
    !  WRITE(*,*) 'Level automatically matched: ', LEVEL
    ENDIF

    IF ( ALLOCATED(RULES(I)%MATCHER%PARAM_ID) ) THEN
      LTMP = ANY( RULES(I)%MATCHER%PARAM_ID .EQ. PARAM_ID )
      MATCH = MATCH .AND. LTMP
    !  WRITE(*,*) 'ParamId matched: ', LTMP, PARAM_ID
    !ELSE
    !  WRITE(*,*) 'ParamId automatically matched: ', PARAM_ID
    ENDIF

    ! Update match counter
    ! WRITE(*,*) 'Match: ', MATCH
    ! WRITE(*,*) '-------------------------------------------------------'
    IF ( MATCH ) THEN
      CNT = CNT + 1
      RID = I
      WRITE(CI,'(I10)') I
      IF ( CNT .EQ. 1 ) THEN
        RMATCH = TRIM(RMATCH)//'{id='//TRIM(ADJUSTL(CI))//', name="'//TRIM(ADJUSTL(RULES(I)%NAME))//'"}'
      ELSE
        RMATCH = TRIM(RMATCH)//', '//'{id='//TRIM(ADJUSTL(CI))//', name="'//TRIM(ADJUSTL(RULES(I)%NAME))//'"}'
      ENDIF
    ENDIF

  ENDDO

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( CNT.LT.1, 2 )
  PP_DEBUG_CRITICAL_COND_THROW( CNT.GT.1, 3 )

  ! Associate the rule definitions to the output variable
  DEFINITIONS = RULES(RID)%DEFINITIONS

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=1024) :: TMP1
    CHARACTER(LEN=1024) :: TMP2

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Rules not allocated' )
    CASE (2)
      WRITE(TMP1,'(A1,I8,A2,I8,A2,I8,A2,I8,A1)') &
&          ' ( paramID=', PARAM_ID, &
&          ', levtype="', ILEVTYPE2CLEVTYPE(LEV_TYPE), &
&          '", repres="', IREPRES2CREPRES(REPRES), &
&          '", level=', LEVEL, ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'No match found for field: '//TRIM(TMP1) )
    CASE (3)
      RMATCH = TRIM(RMATCH)//' )'
      WRITE(TMP2,'(I8)') CNT
      WRITE(TMP1,'(A1,I8,A2,I8,A2,I8,A2,I8,A1)') &
&          ' ( paramId=', PARAM_ID, &
&          ', levtype="', ILEVTYPE2CLEVTYPE(LEV_TYPE), &
&          '", repres="', IREPRES2CREPRES(REPRES), &
&          '", level=', LEVEL, ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, TRIM(ADJUSTL(TMP2))//' matches rules=>'//TRIM(ADJUSTL(RMATCH))//' found for field: '//TRIM(TMP1) )
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


END SUBROUTINE MATCH_RULES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE YAML_RULES_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
