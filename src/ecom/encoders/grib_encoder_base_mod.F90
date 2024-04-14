!> @file grib_encoder_base_mod.F90
!>
!> @brief Definition of an abstract encoder.
!>
!> @author Mirco Valentini
!> @date January 12, 2024

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'grib_encoder_base_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB_ENCODER_BASE_MOD'
MODULE GRIB_ENCODER_BASE_MOD

IMPLICIT NONE

! Default visibility
PRIVATE

!>
!> @brief Definition of the abstract interface GRIB_ENCODER_A.
!>
!> This abstract interface defines the structure for encoding data into the
!> GRIB format. It serves as the foundation for various GRIB encoder implementations.
!> The encoder manages preset samples, their associated metadata, and provides
!> methods for initialization, preset operations, and finalization.
!>
TYPE, ABSTRACT :: GRIB_ENCODER_A

CONTAINS

  !> @brief Initializes and performs preset operations on samples.
  PROCEDURE(INITIALISE_IF), DEFERRED, PUBLIC, PASS :: INITIALISE

  !> @brief Finalizes and performs cleanup of the encoder.
  PROCEDURE(FINALISE_IF),   DEFERRED, PUBLIC, PASS :: FINALISE

  !> @brief Encoder function specific to each encoder implementation.
  PROCEDURE(ENCODE_ATM_IF), DEFERRED, PUBLIC, PASS :: ENCODE_ATM
  PROCEDURE(ENCODE_WAM_IF), DEFERRED, PUBLIC, PASS :: ENCODE_WAM

END TYPE


! Public general interfaces
ABSTRACT INTERFACE

!> @brief Encode fields following specific rules that depends on
!>        the specific implementation
!>
!> @param [inout] this           The object to be initialized.
!> @param [inout] MODEL_PARAMS   Parameters of the model
!>
SUBROUTINE INITIALISE_IF( THIS, MODEL_PARAMS, METADATA_KIND, MIOH )
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T
  USE :: MULTIO_API,  ONLY: MULTIO_HANDLE
  IMPORT :: GRIB_ENCODER_A
IMPLICIT NONE
  ! Dummy arguments
  CLASS(GRIB_ENCODER_A),         INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),              INTENT(IN)    :: METADATA_KIND
  TYPE(MODEL_PAR_T),             INTENT(IN)    :: MODEL_PARAMS
  TYPE(MULTIO_HANDLE), OPTIONAL, INTENT(IN)    :: MIOH

END SUBROUTINE INITIALISE_IF


!> @brief Encode fields following specific rules that depends on
!>        the specific implementation
!>
!> @param [inout] this The object to be initialized.
!>
SUBROUTINE FINALISE_IF( THIS )
  IMPORT :: GRIB_ENCODER_A
IMPLICIT NONE
  ! Dummy arguments
  CLASS(GRIB_ENCODER_A), INTENT(INOUT) :: THIS
END SUBROUTINE FINALISE_IF

!> @brief Encode fields following specific rules that depends on
!>        the specific implementation
!>
!> @param [inout] this           The object to be initialized.
!> @param [inout] MODEL_PARAMS   Parameters of the model
!> @param [inout] GRIB_INFO      Grib information about the field to be encoded
!> @param [inout] MSG            All the informations specifica to the current field
!> @param [inout] METADATA       Handle to be modified by the encoder
!>
FUNCTION ENCODE_ATM_IF( THIS, MODEL_PARAMS, GRIB_INFO, &
& TIME_HIST, CURR_TIME, MSG, METADATA ) RESULT(EX)
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,        ONLY: CURR_TIME_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A
  IMPORT :: GRIB_ENCODER_A
IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_ENCODER_A),           INTENT(INOUT) :: THIS
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Function result
  LOGICAL :: EX
END FUNCTION ENCODE_ATM_IF



!> @brief Encode fields following specific rules that depends on
!>        the specific implementation
!>
!> @param [inout] this           The object to be initialized.
!> @param [inout] MODEL_PARAMS   Parameters of the model
!> @param [inout] GRIB_INFO      Grib information about the field to be encoded
!> @param [inout] MSG            All the informations specifica to the current field
!> @param [inout] METADATA       Handle to be modified by the encoder
!>
FUNCTION ENCODE_WAM_IF( THIS, MODEL_PARAMS, GRIB_INFO, &
& TIME_HIST, CURR_TIME, MSG, METADATA ) RESULT(EX)
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,        ONLY: CURR_TIME_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: OM_CORE_MOD,        ONLY: OM_WAM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A
  IMPORT :: GRIB_ENCODER_A
IMPLICIT NONE
  ! Dummy arguments
  CLASS(GRIB_ENCODER_A),           INTENT(INOUT) :: THIS
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_WAM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  ! Function result
  LOGICAL :: EX
END FUNCTION ENCODE_WAM_IF
END INTERFACE


! White list of symbol visibility (Datatypes)
PUBLIC :: GRIB_ENCODER_A

END MODULE GRIB_ENCODER_BASE_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
