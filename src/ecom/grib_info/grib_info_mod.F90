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
#define PP_FILE_NAME 'grib_info_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB_INFO_MOD'
MODULE GRIB_INFO_MOD

IMPLICIT NONE

! Default visibility of the module
PRIVATE

!  Whitelist of public variables (precedures)
PUBLIC :: SUGRIB_INFO_YAML
PUBLIC :: GRIB_INFO_FREE
PUBLIC :: GRIB_INFO_GET
PUBLIC :: GRIB_INFO_PRINT

CONTAINS


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_INFO_PRINT'
SUBROUTINE GRIB_INFO_PRINT( GI, LOGUNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_CORE_MOD,          ONLY: GRIB_INFO_T
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

  WRITE(LOGUNIT,'(A)')     ' GRIB INFO'
  WRITE(LOGUNIT,'(A)')     ' ---------'
  WRITE(LOGUNIT,'(A,I8)')  ' + ParamId...............................: ', GI%PARAM_ID_
  WRITE(LOGUNIT,'(A,A)')   ' + Short Name............................: ', TRIM(GI%SHORT_NAME_)
  WRITE(LOGUNIT,'(A,A)')   ' + Description...........................: ', TRIM(GI%DESCRIPTION_)
  WRITE(LOGUNIT,'(A,A)')   ' + Measure Unit..........................: ', TRIM(GI%MEASURE_UNIT_)
  WRITE(LOGUNIT,'(A,L)')   ' + Has grib1.............................: ', GI%HAS_GRIB1_
  WRITE(LOGUNIT,'(A,L)')   ' + Has grib2.............................: ', GI%HAS_GRIB2_
  WRITE(LOGUNIT,'(A,7I8)') ' + Requested encoding....................: ', GI%REQUESTED_ENCODING_
  WRITE(LOGUNIT,'(A,7I8)') ' + Bits per values.......................: ', GI%BITS_PER_VALUE_
  WRITE(LOGUNIT,'(A,I8)')  ' + Product definition template number 0..: ', GI%PRODUCT_DEFINITION_TEMPLATE_NUMBER0_
  WRITE(LOGUNIT,'(A,A)')   ' + Type of statistical process 0.........: ', TRIM(TYPE_OF_STATISTICAL_PROCESS_TO_STRING(GI%TYPE_OF_STATISTICAL_PROCESS0_))
  WRITE(LOGUNIT,'(A,A)')   ' + Type of timerange 0...................: ', TRIM(TYPE_OF_TIME_RANGE_TO_STRING(GI%TYPE_OF_TIME_RANGE0_))
  WRITE(LOGUNIT,'(A,I8)')  ' + Overall length of timerange 0.........: ', GI%OVERALL_LENGTH_OF_TIME_RANGE0_
  WRITE(LOGUNIT,'(A,L)')   ' + Is step 0 valid.......................: ', GI%IS_STEP_VALID0_
  WRITE(LOGUNIT,'(A,I8)')  ' + Product definition template number....: ', GI%PRODUCT_DEFINITION_TEMPLATE_NUMBER_
  WRITE(LOGUNIT,'(A,A)')   ' + Type of statistical process...........: ', TRIM(TYPE_OF_STATISTICAL_PROCESS_TO_STRING(GI%TYPE_OF_STATISTICAL_PROCESS_))
  WRITE(LOGUNIT,'(A,A)')   ' + Type of timerange.....................: ', TRIM(TYPE_OF_TIME_RANGE_TO_STRING(GI%TYPE_OF_TIME_RANGE_))
  WRITE(LOGUNIT,'(A,I8)')  ' + Overall length of timerange...........: ', GI%OVERALL_LENGTH_OF_TIME_RANGE_
  WRITE(LOGUNIT,'(A,I8)')  ' + Itop..................................: ', GI%IBOT_
  WRITE(LOGUNIT,'(A,I8)')  ' + Ibot..................................: ', GI%ITOP_

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE GRIB_INFO_PRINT
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
!> @see GRIB_INFO_FREE
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_INFO_GET'
SUBROUTINE GRIB_INFO_GET( GRIB_ID, GI )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: GRIB_INFO_T
  USE :: OM_CORE_MOD, ONLY: LOOKUP_TABLE
  USE :: OM_CORE_MOD, ONLY: GRIB_INFO_DB
  USE :: OM_CORE_MOD, ONLY: OM_SET_CURRENT_GRIB_INFO

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: GRIB_ID
  TYPE(GRIB_INFO_T), POINTER, INTENT(INOUT) :: GI

  ! Local variables
  INTEGER(KIND=JPIB_K) :: II

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialisation
  II = -9999
  GI => NULL()

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( GRIB_ID .GT. SIZE(LOOKUP_TABLE),  1 )
  PP_DEBUG_DEVELOP_COND_THROW( GRIB_ID .LT. 1,  2 )

  ! Get the index of the grib_info
  II = LOOKUP_TABLE( GRIB_ID )

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( II .LE. 0,  3 )

  ! Get the grib_info
  GI => GRIB_INFO_DB(II)

  ! Add grib info to debug info
  CALL OM_SET_CURRENT_GRIB_INFO( GI, GRIB_INFO_PRINT )

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
      WRITE(CGRIB_ID,'(I12)') GRIB_ID
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Grib_ID out of bounds. Greater than upper bound: '//TRIM(ADJUSTL(CGRIB_ID)) )
    CASE (2)
      CGRIB_ID = REPEAT( ' ', 128 )
      WRITE(CGRIB_ID,'(I12)') GRIB_ID
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Grib_ID out of bounds. Lower than lower bound: '//TRIM(ADJUSTL(CGRIB_ID)) )
    CASE (3)
      CGRIB_ID = REPEAT( ' ', 128 )
      WRITE(CGRIB_ID,'(I12)') GRIB_ID
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

END SUBROUTINE GRIB_INFO_GET
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief function used to initialise the grib informations
!>
!> @see GRIB_INFO_FREE
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SUGRIB_INFO_YAML'
SUBROUTINE SUGRIB_INFO_YAML( CFG, PROCESSOR_TOPO, MODEL_PARAMS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T

  USE :: OM_CORE_MOD, ONLY: LOOKUP_TABLE
  USE :: OM_CORE_MOD, ONLY: LOOKUP_TABLE_BWD
  USE :: OM_CORE_MOD, ONLY: GRIB_INFO_DB

  USE :: TIME_ASSUMPTIONS_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_TO_STRING
  USE :: TIME_ASSUMPTIONS_MOD, ONLY: TYPE_OF_TIME_RANGE_TO_STRING

  USE :: TIME_ASSUMPTIONS_MOD,      ONLY: TIME_ASSUMPTIONS_INIT
  USE :: PACKAGING_ASSUMPTIONS_MOD, ONLY: PACKAGING_ASSUMPTIONS_INIT
  USE :: LEVEL_ASSUMPTIONS_MOD,     ONLY: LEVEL_ASSUMPTIONS_INIT
  USE :: GENERAL_ASSUMPTIONS_MOD,   ONLY: GENERAL_ASSUMPTIONS_INIT

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

  ! Local variables
  TYPE(FCKIT_CONFIGURATION), ALLOCATABLE, DIMENSION(:) :: RECORDS
  CHARACTER(LEN=:), ALLOCATABLE :: CLTMP
  TYPE(FCKIT_CONFIGURATION) :: PACK
  INTEGER(KIND=JPIB_K) :: NUM_LINES
  INTEGER(KIND=JPIB_K) :: MAX_PAR
  INTEGER(KIND=JPIB_K) :: UNIT
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: PAR
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialize line counter
  NUM_LINES = 0
  MAX_PAR = 0
  UNIT = -9999
  STAT = -9999
  CNT = -9999

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( ALLOCATED(LOOKUP_TABLE), 1 )
  PP_DEBUG_DEVELOP_COND_THROW( ALLOCATED(LOOKUP_TABLE_BWD), 2 )
  PP_DEBUG_DEVELOP_COND_THROW( ALLOCATED(GRIB_INFO_DB), 3 )

  ! Initialize assumptions
  CALL TIME_ASSUMPTIONS_INIT( CFG, MODEL_PARAMS )
  CALL PACKAGING_ASSUMPTIONS_INIT( CFG, MODEL_PARAMS )
  CALL LEVEL_ASSUMPTIONS_INIT( CFG, MODEL_PARAMS )
  CALL GENERAL_ASSUMPTIONS_INIT( CFG, MODEL_PARAMS )

  ! Get the rank inside the IO server
  IF ( CFG%GET( 'grib-info', RECORDS ) ) THEN


    ! Read the file line by line
    NUM_LINES = SIZE(RECORDS)
    ReadParametersSize: DO CNT = 1, NUM_LINES

      EX = RECORDS(CNT)%GET( 'param-id', PAR  )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, 4 )

      ! Read nex line
      IF (  PAR .GT. MAX_PAR ) THEN
        MAX_PAR = PAR
      ENDIF

    ENDDO ReadParametersSize

    ! Error handling
    PP_DEBUG_DEVELOP_COND_THROW( NUM_LINES.LT.0, 5 )
    PP_DEBUG_DEVELOP_COND_THROW( MAX_PAR.LT.0,   6 )

    ! Allocate the buffers
    ALLOCATE( GRIB_INFO_DB( NUM_LINES ) )
    ALLOCATE( LOOKUP_TABLE( MAX_PAR ) )
    ALLOCATE( LOOKUP_TABLE_BWD( NUM_LINES ) )

    LOOKUP_TABLE = -9999
    LOOKUP_TABLE_BWD = -9999

    ReadParameters: DO CNT = 1, NUM_LINES

      EX = RECORDS(CNT)%GET( 'param-id', PAR  )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, 7 )

      ! Update loouptables
      LOOKUP_TABLE(PAR) = CNT
      LOOKUP_TABLE_BWD(CNT) = PAR

      ! Read the grib informations
      CALL READ_GRIB_INFO_YAML( RECORDS(CNT), MODEL_PARAMS, CNT, PAR )

    ENDDO ReadParameters

  ENDIF

  ! Free memory used by records
  IF ( ALLOCATED(RECORDS) ) THEN
    DO CNT = 1, NUM_LINES
      CALL RECORDS(CNT)%FINAL()
    ENDDO
    DEALLOCATE( RECORDS )
  ENDIF

  OPEN( NEWUNIT=UNIT, FILE='bitsPerValue.log', ACTION='WRITE' )
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
  WRITE(UNIT,'(A)')    '#  M  :: Model level'
  WRITE(UNIT,'(A)')    '#  P  :: Pressure level'
  WRITE(UNIT,'(A)')    '#  V  :: Vorticity level'
  WRITE(UNIT,'(A)')    '#  T  :: Theta level'
  WRITE(UNIT,'(A)')    '#  S  :: Surface'
  WRITE(UNIT,'(A)')    '#  WI :: Wave model intgral fields'
  WRITE(UNIT,'(A)')    '#  WS :: Wave model spectra'
  WRITE(UNIT,'(A)')    '#  '
  WRITE(UNIT,'(A,A)')  '#',REPEAT('-',136)
  WRITE(UNIT,'(A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1)') '|', &
&  ' PARAM ID.      ', '|', &
&  ' M              ', '|', &
&  ' P              ', '|', &
&  ' V              ', '|', &
&  ' T              ', '|', &
&  ' S              ', '|', &
&  ' WI             ', '|', &
&  ' WS             ', '|'
  WRITE(UNIT,'(A,A)')  '#',REPEAT('-',136)
  DO CNT = 1, SIZE(GRIB_INFO_DB)
    WRITE(UNIT,'(A1,I16,A1,I16,A1,I16,A1,I16,A1,I16,A1,I16,A1,I16,A1,I16,A1)') '|', &
&     GRIB_INFO_DB(CNT)%PARAM_ID_,          '|', &
&     GRIB_INFO_DB(CNT)%BITS_PER_VALUE_(1), '|', &
&     GRIB_INFO_DB(CNT)%BITS_PER_VALUE_(2), '|', &
&     GRIB_INFO_DB(CNT)%BITS_PER_VALUE_(3), '|', &
&     GRIB_INFO_DB(CNT)%BITS_PER_VALUE_(4), '|', &
&     GRIB_INFO_DB(CNT)%BITS_PER_VALUE_(5), '|', &
&     GRIB_INFO_DB(CNT)%BITS_PER_VALUE_(6), '|', &
&     GRIB_INFO_DB(CNT)%BITS_PER_VALUE_(7), '|'
  WRITE(UNIT,'(A,A)')  '#',REPEAT('-',136)
  ENDDO
  CLOSE(UNIT)

  OPEN( NEWUNIT=UNIT, FILE='timeManagement.log', ACTION='WRITE' )
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
  WRITE(UNIT,'(A,A)')  '#', REPEAT('-',170)
  WRITE(UNIT,'(A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1)') '|', &
&  ' PARAM ID.      ', '|', &
&  ' P.D.T. (t=0)   ', '|', &
&  ' T.S.P. (t=0)   ', '|', &
&  ' T.T.R. (t=0)   ', '|', &
&  ' L.T.R. (t=0)[s]', '|', &
&  ' EMIT STEP 0    ', '|', &
&  ' P.D.T.         ', '|', &
&  ' T.S.P.         ', '|', &
&  ' T.T.R.         ', '|', &
&  ' L.T.R. [s]     ', '|'
  WRITE(UNIT,'(A,A)')  '#', REPEAT('-',170)
  DO CNT = 1, SIZE(GRIB_INFO_DB)
    WRITE(UNIT,'(A1,I16,A1,I16,A1,A16,A1,A16,A1,I16,A1,L16,A1,I16,A1,A16,A1,A16,A1,I16,A1)') '|',&
&    GRIB_INFO_DB(CNT)%PARAM_ID_ , '|',&
&    GRIB_INFO_DB(CNT)%PRODUCT_DEFINITION_TEMPLATE_NUMBER0_ , '|',&
&    TRIM(TYPE_OF_STATISTICAL_PROCESS_TO_STRING(GRIB_INFO_DB(CNT)%TYPE_OF_STATISTICAL_PROCESS0_)) , '|',&
&    TRIM(TYPE_OF_TIME_RANGE_TO_STRING(GRIB_INFO_DB(CNT)%TYPE_OF_TIME_RANGE0_)) , '|',&
&    GRIB_INFO_DB(CNT)%OVERALL_LENGTH_OF_TIME_RANGE0_ , '|',&
&    GRIB_INFO_DB(CNT)%IS_STEP_VALID0_ , '|',&
&    GRIB_INFO_DB(CNT)%PRODUCT_DEFINITION_TEMPLATE_NUMBER_ , '|',&
&    TRIM(TYPE_OF_STATISTICAL_PROCESS_TO_STRING(GRIB_INFO_DB(CNT)%TYPE_OF_STATISTICAL_PROCESS_)) , '|',&
&    TRIM(TYPE_OF_TIME_RANGE_TO_STRING(GRIB_INFO_DB(CNT)%TYPE_OF_TIME_RANGE_)), '|',&
&    GRIB_INFO_DB(CNT)%OVERALL_LENGTH_OF_TIME_RANGE_, '|'
  WRITE(UNIT,'(A,A)')  '#', REPEAT('-',170)
  ENDDO
  CLOSE(UNIT)

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Lookup table already allocated' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Inv. lookup table already allocated' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'GribInfo already allocated' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to open file' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'NUM_LINES lower than 0' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'MAX_GRIB_IDX lower than 0' )
    CASE (7)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read param Id' )
    CASE (8)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read enable-compression' )
    CASE (9)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'enable-compression not allocated after reading' )
    CASE (10)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'enable compression too long' )
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

END SUBROUTINE SUGRIB_INFO_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief function used to read grib inforamtions given a specific parameter
!>
!> @param [in] param IDX of the file to be loaded
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_GRIB_INFO_YAML'
SUBROUTINE READ_GRIB_INFO_YAML( CFG, MODEL_PARAMS, CNT, IDX )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD, ONLY: UNDEF_PARAM_E

  USE :: OM_CORE_MOD, ONLY: GRIB_INFO_DB

  USE :: OM_CORE_MOD, ONLY: MODEL_LEVEL_E
  USE :: OM_CORE_MOD, ONLY: PRESSURE_LEVEL_E
  USE :: OM_CORE_MOD, ONLY: VORTICITY_LEVEL_E
  USE :: OM_CORE_MOD, ONLY: THETA_LEVEL_E
  USE :: OM_CORE_MOD, ONLY: SURFACE_E
  USE :: OM_CORE_MOD, ONLY: WAVE_INT_E
  USE :: OM_CORE_MOD, ONLY: WAVE_SPEC_E

  USE :: TIME_ASSUMPTIONS_MOD,      ONLY: COMPUTE_TIMING_ZERO
  USE :: TIME_ASSUMPTIONS_MOD,      ONLY: COMPUTE_TIMING
  USE :: PACKAGING_ASSUMPTIONS_MOD, ONLY: COMPUTE_BITS_PER_VALUE_DEFAULT
  USE :: LEVEL_ASSUMPTIONS_MOD,     ONLY: COMPUTE_TOPBOT


  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy argumants
  TYPE(FCKIT_CONFIGURATION), INTENT(IN) :: CFG
  TYPE(MODEL_PAR_T),         INTENT(IN) :: MODEL_PARAMS
  INTEGER(KIND=JPIB_K),      INTENT(IN) :: CNT
  INTEGER(KIND=JPIB_K),      INTENT(IN) :: IDX

  ! Local variables
  LOGICAL :: EX
  INTEGER(KIND=JPIB_K)  :: STAT
  INTEGER(KIND=JPIB_K)  :: ITMP
  INTEGER(KIND=JPIB_K), ALLOCATABLE, DIMENSION(:) :: TMP
  CHARACTER(len=:), ALLOCATABLE :: CTMP
  LOGICAL :: LTMP

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( .NOT. ALLOCATED(GRIB_INFO_DB),  1 )

  GRIB_INFO_DB(CNT)%PARAM_ID_ = IDX

  ! Read the requested encoding
  IF ( CFG%HAS( 'requested-encoding' ) ) THEN
    IF (ALLOCATED(TMP)) DEALLOCATE(TMP)
    EX = CFG%GET( 'requested-encoding', TMP )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, 1 )
    IF ( SIZE(TMP) .EQ. 7 ) THEN
      GRIB_INFO_DB(CNT)%REQUESTED_ENCODING_ = TMP
      DEALLOCATE(TMP)
    ELSE
      DEALLOCATE(TMP)
      PP_DEBUG_CRITICAL_THROW( 2 )
    ENDIF
  ENDIF

  ! Read Bits per Value
  IF ( CFG%HAS( 'bits-per-value' ) ) THEN
    IF (ALLOCATED(TMP)) DEALLOCATE(TMP)
    EX = CFG%GET( 'bits-per-value', TMP )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, 3 )
    IF ( SIZE(TMP) .EQ. 7 ) THEN
      GRIB_INFO_DB(CNT)%BITS_PER_VALUE_ = TMP
      DEALLOCATE(TMP)
    ELSE
      DEALLOCATE(TMP)
      PP_DEBUG_CRITICAL_THROW( 4 )
    ENDIF
  ENDIF

  ! Read has_grib_1
  IF ( CFG%HAS( 'has-grib-1' ) ) THEN
    EX = CFG%GET( 'has-grib-1', LTMP )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, 5 )
    GRIB_INFO_DB(CNT)%HAS_GRIB1_ = LTMP
  ELSE
    PP_DEBUG_CRITICAL_THROW( 6 )
  ENDIF

  ! Read has_grib_2
  IF ( CFG%HAS( 'has-grib-2' ) ) THEN
    EX = CFG%GET( 'has-grib-2', LTMP )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, 7 )
    GRIB_INFO_DB(CNT)%HAS_GRIB2_ = LTMP
  ELSE
    PP_DEBUG_CRITICAL_THROW( 8 )
  ENDIF


  ! Read short name
  IF ( CFG%HAS( 'short-name' ) ) THEN
    IF ( ALLOCATED(CTMP) ) DEALLOCATE(CTMP)
    EX = CFG%GET( 'short-name', CTMP )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, 9 )
    IF ( ALLOCATED(CTMP) .AND. LEN(CTMP) .LE. 32 ) THEN
      GRIB_INFO_DB(CNT)%SHORT_NAME_ = CTMP(:)
      DEALLOCATE(CTMP)
    ELSE
      DEALLOCATE(CTMP)
      PP_DEBUG_CRITICAL_THROW( 10 )
    ENDIF
  ELSE
    PP_DEBUG_CRITICAL_THROW( 11 )
  ENDIF

  ! Read measureUnit
  IF ( CFG%HAS( 'measure-unit' ) ) THEN
    IF ( ALLOCATED(CTMP) ) DEALLOCATE(CTMP)
    EX = CFG%GET( 'measure-unit', CTMP )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, 12 )
    IF ( ALLOCATED(CTMP) .AND. LEN(CTMP) .LE. 32 ) THEN
      GRIB_INFO_DB(CNT)%MEASURE_UNIT_ = CTMP(:)
      DEALLOCATE(CTMP)
    ELSE
      DEALLOCATE(CTMP)
      PP_DEBUG_CRITICAL_THROW( 13 )
    ENDIF
  ELSE
    PP_DEBUG_CRITICAL_THROW( 14 )
  ENDIF

  ! Read description
  IF ( CFG%HAS( 'description' ) ) THEN
    IF ( ALLOCATED(CTMP) ) DEALLOCATE(CTMP)
    EX = CFG%GET( 'description', CTMP )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, 15 )
    IF ( ALLOCATED(CTMP) .AND. LEN(CTMP) .LE. 1024 ) THEN
      GRIB_INFO_DB(CNT)%DESCRIPTION_ = CTMP(:)
      DEALLOCATE(CTMP)
    ELSE
      DEALLOCATE(CTMP)
      PP_DEBUG_CRITICAL_THROW( 16 )
    ENDIF
  ELSE
    PP_DEBUG_CRITICAL_THROW( 17 )
  ENDIF


  ! Precompute the search for bottom and top levels of multilevel surfaces
  CALL COMPUTE_TOPBOT( MODEL_PARAMS, GRIB_INFO_DB(CNT)%PARAM_ID_, GRIB_INFO_DB(CNT)%ITOP_, GRIB_INFO_DB(CNT)%IBOT_ )


  ! If the bits per value is not passed from outside, then apply the defaul value
  IF ( GRIB_INFO_DB(CNT)%BITS_PER_VALUE_(MODEL_LEVEL_E).EQ.UNDEF_PARAM_E ) THEN
    GRIB_INFO_DB(CNT)%BITS_PER_VALUE_(MODEL_LEVEL_E) = &
&      COMPUTE_BITS_PER_VALUE_DEFAULT( MODEL_PARAMS, GRIB_INFO_DB(CNT)%PARAM_ID_, MODEL_LEVEL_E )
  ENDIF
  IF ( GRIB_INFO_DB(CNT)%BITS_PER_VALUE_(PRESSURE_LEVEL_E).EQ.UNDEF_PARAM_E ) THEN
    GRIB_INFO_DB(CNT)%BITS_PER_VALUE_(PRESSURE_LEVEL_E) = &
&      COMPUTE_BITS_PER_VALUE_DEFAULT( MODEL_PARAMS, GRIB_INFO_DB(CNT)%PARAM_ID_, PRESSURE_LEVEL_E )
  ENDIF
  IF ( GRIB_INFO_DB(CNT)%BITS_PER_VALUE_(VORTICITY_LEVEL_E).EQ.UNDEF_PARAM_E ) THEN
    GRIB_INFO_DB(CNT)%BITS_PER_VALUE_(VORTICITY_LEVEL_E) = &
&      COMPUTE_BITS_PER_VALUE_DEFAULT( MODEL_PARAMS, GRIB_INFO_DB(CNT)%PARAM_ID_, VORTICITY_LEVEL_E )
  ENDIF
  IF ( GRIB_INFO_DB(CNT)%BITS_PER_VALUE_(THETA_LEVEL_E).EQ.UNDEF_PARAM_E ) THEN
    GRIB_INFO_DB(CNT)%BITS_PER_VALUE_(THETA_LEVEL_E) = &
&      COMPUTE_BITS_PER_VALUE_DEFAULT( MODEL_PARAMS, GRIB_INFO_DB(CNT)%PARAM_ID_, THETA_LEVEL_E )
  ENDIF
  IF ( GRIB_INFO_DB(CNT)%BITS_PER_VALUE_(SURFACE_E).EQ.UNDEF_PARAM_E ) THEN
    GRIB_INFO_DB(CNT)%BITS_PER_VALUE_(SURFACE_E) = &
&      COMPUTE_BITS_PER_VALUE_DEFAULT( MODEL_PARAMS, GRIB_INFO_DB(CNT)%PARAM_ID_, SURFACE_E )
  ENDIF
  IF ( GRIB_INFO_DB(CNT)%BITS_PER_VALUE_(WAVE_INT_E).EQ.UNDEF_PARAM_E ) THEN
    GRIB_INFO_DB(CNT)%BITS_PER_VALUE_(WAVE_INT_E) = &
&      COMPUTE_BITS_PER_VALUE_DEFAULT( MODEL_PARAMS, GRIB_INFO_DB(CNT)%PARAM_ID_, WAVE_INT_E )
  ENDIF
  IF ( GRIB_INFO_DB(CNT)%BITS_PER_VALUE_(WAVE_SPEC_E).EQ.UNDEF_PARAM_E ) THEN
    GRIB_INFO_DB(CNT)%BITS_PER_VALUE_(WAVE_SPEC_E) = &
&      COMPUTE_BITS_PER_VALUE_DEFAULT( MODEL_PARAMS, GRIB_INFO_DB(CNT)%PARAM_ID_, WAVE_SPEC_E )
  ENDIF

  ! Apply the default options for timing informations
  CALL COMPUTE_TIMING_ZERO( &
&  MODEL_PARAMS, &
&  GRIB_INFO_DB(CNT)%PARAM_ID_, &
&  GRIB_INFO_DB(CNT)%PRODUCT_DEFINITION_TEMPLATE_NUMBER0_, &
&  GRIB_INFO_DB(CNT)%TYPE_OF_STATISTICAL_PROCESS0_, &
&  GRIB_INFO_DB(CNT)%TYPE_OF_TIME_RANGE0_, &
&  GRIB_INFO_DB(CNT)%OVERALL_LENGTH_OF_TIME_RANGE0_, &
&  GRIB_INFO_DB(CNT)%IS_STEP_VALID0_ )

  CALL COMPUTE_TIMING( &
&  MODEL_PARAMS, &
&  GRIB_INFO_DB(CNT)%PARAM_ID_, &
&  GRIB_INFO_DB(CNT)%PRODUCT_DEFINITION_TEMPLATE_NUMBER_, &
&  GRIB_INFO_DB(CNT)%TYPE_OF_STATISTICAL_PROCESS_, &
&  GRIB_INFO_DB(CNT)%TYPE_OF_TIME_RANGE_, &
&  GRIB_INFO_DB(CNT)%OVERALL_LENGTH_OF_TIME_RANGE_ )


  IF ( ALLOCATED(TMP) ) THEN
    DEALLOCATE(TMP)
  ENDIF
  IF ( ALLOCATED(CTMP) ) THEN
    DEALLOCATE(CTMP)
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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'failed to read requestedEncoding' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'requestedEncoding not defined' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'failed to read bitsPerValue' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'bitsPerValue ot defined' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'failed to read hasGrib1' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'hasGrib1 not defined' )
    CASE (7)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'failed to read hasGrib2' )
    CASE (8)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'hasGrib2 not defined' )
    CASE (9)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'failed to read shortName' )
    CASE (10)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'shortName too long' )
    CASE (11)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'shortName not defined' )
    CASE (12)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'failed to read unitMeasure' )
    CASE (13)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'unitMeasure too long' )
    CASE (14)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'unitMeasure not defined' )
    CASE (15)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'failed to read description' )
    CASE (16)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'description too long' )
    CASE (17)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'description not defined' )
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

END SUBROUTINE READ_GRIB_INFO_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

!>
!> @brief free all the grib informations
!>
!> @param [in] param IDX of the file to be loaded
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_INFO_FREE'
SUBROUTINE GRIB_INFO_FREE(  )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: LOOKUP_TABLE
  USE :: OM_CORE_MOD, ONLY: LOOKUP_TABLE_BWD
  USE :: OM_CORE_MOD, ONLY: GRIB_INFO_DB

  USE :: TIME_ASSUMPTIONS_MOD,      ONLY: TIME_ASSUMPTIONS_FREE
  USE :: PACKAGING_ASSUMPTIONS_MOD, ONLY: PACKAGING_ASSUMPTIONS_FREE
  USE :: LEVEL_ASSUMPTIONS_MOD,     ONLY: LEVEL_ASSUMPTIONS_FREE
  USE :: GENERAL_ASSUMPTIONS_MOD,   ONLY: GENERAL_ASSUMPTIONS_FREE

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  IF ( ALLOCATED(LOOKUP_TABLE) ) THEN
    DEALLOCATE(LOOKUP_TABLE)
  ENDIF


  IF ( ALLOCATED(LOOKUP_TABLE_BWD) ) THEN
    DEALLOCATE(LOOKUP_TABLE_BWD)
  ENDIF


  IF ( ALLOCATED(GRIB_INFO_DB) ) THEN
    DEALLOCATE(GRIB_INFO_DB)
  ENDIF

  ! Free assumptions configuration
  CALL TIME_ASSUMPTIONS_FREE()
  CALL LEVEL_ASSUMPTIONS_FREE()
  CALL PACKAGING_ASSUMPTIONS_FREE()
  CALL GENERAL_ASSUMPTIONS_FREE()

  ! Exit point on error
  RETURN

END SUBROUTINE GRIB_INFO_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE GRIB_INFO_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
