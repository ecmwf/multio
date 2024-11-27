!> @file map_mod.F90
!>
!> @brief Module containing the implementation of a Red Black tree.
!>
!> Implementation of a Red Black tree.
!> Every routine in this file is deeply explained in the book:
!> "Introduction to Algorithms"
!> { Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest, Clifford Stein}
!>
!> @todo improve error handling
!>
!> @author Mirco Valentini
!> @date   January 31, 2024
!>

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'mapping_options_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'MAPPING_OPTIONS_MOD'
MODULE MAPPING_OPTIONS_MOD

IMPLICIT NONE

!> @brief Default visibility of the module
PRIVATE

TYPE :: MAPPING_OPTIONS_T
  LOGICAL :: ENABLE_MAPPING = .FALSE.
END TYPE

!> Whitelist of public symbols
PUBLIC :: MAPPING_OPTIONS_T

END MODULE MAPPING_OPTIONS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME