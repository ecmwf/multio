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


#define PP_FILE_NAME 'cache_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'CACHE_UTILS_MOD'
MODULE CACHE_UTILS_MOD
IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!> Options used to control the cache
TYPE :: CACHE_OPTIONS_T
  LOGICAL :: USE_COMPRESSED_CACHE = .TRUE.
  LOGICAL :: ENABLE_CACHE = .FALSE.
  LOGICAL :: VERBOSE = .FALSE.
END TYPE

PUBLIC :: CACHE_OPTIONS_T

END MODULE CACHE_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME