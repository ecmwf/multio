!> @file grib_info_data_mod.F90
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
#define PP_FILE_NAME 'grib_info_data_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB_INFO_DATA_MOD'
MODULE GRIB_INFO_DATA_MOD

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT16
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD,   ONLY: JPIB_K
  USE :: OM_ENUMERATORS_MOD, ONLY: UNDEF_PARAM_E

IMPLICIT NONE

! Default visibility of the module
PRIVATE


!> @brief class used to store all the grib informations used for encoding
TYPE :: GRIB_INFO_T
  INTEGER(KIND=JPIB_K)  :: PARAM_ID_ = UNDEF_PARAM_E
  CHARACTER(LEN=32)     :: SHORT_NAME_   = REPEAT(' ',32)
  CHARACTER(LEN=1024)   :: DESCRIPTION_  = REPEAT(' ',1024)
  CHARACTER(LEN=32)     :: MEASURE_UNIT_ = REPEAT(' ',32)
  LOGICAL               :: HAS_GRIB1_ = .FALSE.
  LOGICAL               :: HAS_GRIB2_ = .FALSE.
  INTEGER(KIND=JPIB_K), DIMENSION(7) :: REQUESTED_ENCODING_ = [UNDEF_PARAM_E,UNDEF_PARAM_E,UNDEF_PARAM_E,UNDEF_PARAM_E,UNDEF_PARAM_E,UNDEF_PARAM_E,UNDEF_PARAM_E]
  INTEGER(KIND=JPIB_K), DIMENSION(7) :: BITS_PER_VALUE_ = [UNDEF_PARAM_E,UNDEF_PARAM_E,UNDEF_PARAM_E,UNDEF_PARAM_E,UNDEF_PARAM_E,UNDEF_PARAM_E,UNDEF_PARAM_E]
  INTEGER(KIND=JPIB_K)  :: PRODUCT_DEFINITION_TEMPLATE_NUMBER0_ = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K)  :: TYPE_OF_STATISTICAL_PROCESS0_ = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K)  :: TYPE_OF_TIME_RANGE0_ = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K)  :: OVERALL_LENGTH_OF_TIME_RANGE0_ = UNDEF_PARAM_E
  LOGICAL               :: IS_STEP_VALID0_ = .FALSE.
  INTEGER(KIND=JPIB_K)  :: PRODUCT_DEFINITION_TEMPLATE_NUMBER_ = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K)  :: TYPE_OF_STATISTICAL_PROCESS_ = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K)  :: TYPE_OF_TIME_RANGE_ = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K)  :: OVERALL_LENGTH_OF_TIME_RANGE_ = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K)  :: IBOT_ = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K)  :: ITOP_ = UNDEF_PARAM_E
END TYPE

! Grib informations
INTEGER(KIND=INT16), TARGET, DIMENSION(:), ALLOCATABLE :: LOOKUP_TABLE
INTEGER(KIND=INT32), TARGET, DIMENSION(:), ALLOCATABLE :: LOOKUP_TABLE_BWD
TYPE(GRIB_INFO_T),   TARGET, DIMENSION(:), ALLOCATABLE :: GRIB_INFO_DB

! Whitelist of public variables (datatypes)
PUBLIC :: GRIB_INFO_T

! Whitelist of public variables (data)
PUBLIC :: LOOKUP_TABLE
PUBLIC :: LOOKUP_TABLE_BWD
PUBLIC :: GRIB_INFO_DB

END MODULE GRIB_INFO_DATA_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
