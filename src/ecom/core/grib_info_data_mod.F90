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
  USE :: OM_CONSTANTS_MOD,   ONLY: CAPACITY
  USE :: OM_ENUMERATORS_MOD, ONLY: UNDEF_PARAM_E

IMPLICIT NONE

! Default visibility of the module
PRIVATE


TYPE :: GRIB_INFO_T

  INTEGER(KIND=JPIB_K) :: REQUESTED_ENCODING = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: BITS_PER_VALUE = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: PACKING_TYPE = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: ILEV_TYPE = UNDEF_PARAM_E

  LOGICAL              :: IS_STEP0_VALID_ = .FALSE.
  INTEGER(KIND=JPIB_K) :: PRODUCT_DEFINITION_TEMPLATE_NUMBER_ = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: TYPE_OF_STATISTICAL_PROCESS_ = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: TYPE_OF_TIME_RANGE_ = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: OVERALL_LENGTH_OF_TIME_RANGE_ = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: IBOT_ = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: ITOP_ = UNDEF_PARAM_E
  ! Option that needs to be specified per field, but used onli by
  ! "grib-header-to-multio" output manager
  LOGICAL :: DIRECT_TO_FDB = .FALSE.
END TYPE

! Time history datatype
TYPE :: TIME_HISTORY_T
  INTEGER(KIND=JPIB_K) :: SIZE_ = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K), DIMENSION(0:CAPACITY) :: HIST_= UNDEF_PARAM_E
END TYPE

! Grib informations
INTEGER(KIND=INT16), TARGET, DIMENSION(:), ALLOCATABLE :: LOOKUP_TABLE
INTEGER(KIND=INT32), TARGET, DIMENSION(:), ALLOCATABLE :: LOOKUP_TABLE_BWD

! Whitelist of public variables (datatypes)
PUBLIC :: GRIB_INFO_T
PUBLIC :: TIME_HISTORY_T

! Whitelist of public variables (data)
PUBLIC :: LOOKUP_TABLE
PUBLIC :: LOOKUP_TABLE_BWD

END MODULE GRIB_INFO_DATA_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
