!> @file
!>
!> @brief Definition of the `OUTPUT_MANAGER_BASE_MOD` module.
!>
!> The `OUTPUT_MANAGER_BASE_MOD` module provides an abstract interface for output manager base functionality.
!> It defines an abstract type `OUTPUT_MANAGER_BASE_A` with deferred procedures for setup, write, flushing, and finalization.
!>
!> @author Mirco Valentini
!> @date   January 12, 2024

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'output_manager_base_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'OUTUPUT_MANAGER_BASE_MOD'
MODULE OUTUPUT_MANAGER_BASE_MOD

IMPLICIT NONE

! Default visibility
PRIVATE

!> @brief definition of an abstract type `OUTPUT_MANAGER_BASE_A` with deferred procedures for setup, write, flushing, and finalization.
!>
!> The OUTPUT_MANAGER_BASE_A interface serves as a template for different output managers.
TYPE, ABSTRACT :: OUTPUT_MANAGER_BASE_A
CONTAINS

  !> @brief Initializes the object
  PROCEDURE(SETUP_IF), DEFERRED, PASS, PUBLIC :: SETUP

  !> @brief Initializes the object
  PROCEDURE(READ_CFG_FROM_YAML_IF), DEFERRED, PASS, PUBLIC :: READ_CFG_FROM_YAML


  !> @brief Used to write an atmosphere field
  PROCEDURE(WRITE_ATM_DP_IF), DEFERRED, PASS, PUBLIC :: WRITE_ATM_DP
  PROCEDURE(WRITE_ATM_SP_IF), DEFERRED, PASS, PUBLIC :: WRITE_ATM_SP

  GENERIC :: WRITE_ATM => WRITE_ATM_SP, WRITE_ATM_DP


  !> @brief Used to write an atmosphere field
  PROCEDURE(WRITE_WAM_DP_IF), DEFERRED, PASS, PUBLIC :: WRITE_WAM_DP
  PROCEDURE(WRITE_WAM_SP_IF), DEFERRED, PASS, PUBLIC :: WRITE_WAM_SP

  GENERIC :: WRITE_WAM => WRITE_WAM_SP, WRITE_WAM_DP


  !> @brief Used to notify a new step. Depending on the implementation different action can be taken
  PROCEDURE(FLUSH_STEP_IF), DEFERRED, PASS, PUBLIC :: FLUSH_STEP

  !> @brief Used to notify the last step
  PROCEDURE(FLUSH_LAST_STEP_IF), DEFERRED, PASS, PUBLIC :: FLUSH_LAST_STEP

  !> @brief Used to notify a new step and trigger a restart.
  PROCEDURE(FLUSH_STEP_AND_TRIGGER_RESTART_IF), DEFERRED, PASS, PUBLIC :: FLUSH_STEP_AND_TRIGGER_RESTART

  !> @brief Used to finalise the output manager
  PROCEDURE(FINALISE_IF), DEFERRED, PASS, PUBLIC :: FINALISE
END TYPE


ABSTRACT INTERFACE

!>
!> @brief Initializes the object from an instance of the IOserver.
!>
!> This procedure initializes the object using the informations
!> contained in the instance of the IOServer
!>
!> @param [inout] THIS  The object to be initialized.
!> @param [in]    CFG   The YAML configuration object to be readed
!>
SUBROUTINE READ_CFG_FROM_YAML_IF( THIS, CFG )
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION
  IMPORT :: OUTPUT_MANAGER_BASE_A
IMPLICIT NONE
  CLASS(OUTPUT_MANAGER_BASE_A), INTENT(INOUT) :: THIS
  TYPE(FCKIT_CONFIGURATION),    INTENT(IN)    :: CFG
END SUBROUTINE READ_CFG_FROM_YAML_IF

!>
!> @brief Initializes the object from an instance of the IOserver.
!>
!> This procedure initializes the object using the informations
!> contained in the instance of the IOServer
!>
!> @param [inout] this           The object to be initialized.
!> @param [in]    YAMLFNAME      NAme of the YAML main configuration file
!> @param [in]    PROCESSOR_TOPO Processor topology to be used in a multiprocessor run
!> @param [in]    MODEL_PARAMS   Model parameters that are frozen during the simulation
!>
SUBROUTINE SETUP_IF( THIS, YAMLFNAME, PROCESSOR_TOPO, MODEL_PARAMS )
  USE :: OM_CORE_MOD, ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T
  IMPORT :: OUTPUT_MANAGER_BASE_A
IMPLICIT NONE
  CLASS(OUTPUT_MANAGER_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),             INTENT(IN)    :: YAMLFNAME
  TYPE(PROC_TOPO_T), TARGET,    INTENT(IN)    :: PROCESSOR_TOPO
  TYPE(MODEL_PAR_T), TARGET,    INTENT(IN)    :: MODEL_PARAMS
END SUBROUTINE SETUP_IF


!>
!> @brief Write fields contained in a request received by the IOserver.
!>
!> Reads and encodes all the fields present in a received request from the IOserver,
!> and subsequently writes the encoded data using the appropriate sinks.
!>
!> @param [inout] this  The object to be initialized.
!> @param [inout] ydmsg Message to be encoded
!>
SUBROUTINE WRITE_ATM_DP_IF( THIS, YDMSG, VALUES_DP )
  USE :: OM_CORE_MOD, ONLY: JPRD_K
  USE :: OM_CORE_MOD, ONLY: OM_ATM_MSG_T
  IMPORT :: OUTPUT_MANAGER_BASE_A
IMPLICIT NONE
  CLASS(OUTPUT_MANAGER_BASE_A), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_ATM_MSG_T),                   INTENT(IN)    :: YDMSG
  REAL(KIND=JPRD_K), DIMENSION(:),      INTENT(IN)    :: VALUES_DP
END SUBROUTINE WRITE_ATM_DP_IF


!>
!> @brief Write fields contained in a request received by the IOserver.
!>
!> Reads and encodes all the fields present in a received request from the IOserver,
!> and subsequently writes the encoded data using the appropriate sinks.
!>
!> @param [inout] this  The object to be initialized.
!> @param [inout] ydmsg Message to be encoded
!>
SUBROUTINE WRITE_ATM_SP_IF( THIS, YDMSG, VALUES_SP )
  USE :: OM_CORE_MOD, ONLY: JPRM_K
  USE :: OM_CORE_MOD, ONLY: OM_ATM_MSG_T
  IMPORT :: OUTPUT_MANAGER_BASE_A
IMPLICIT NONE
  CLASS(OUTPUT_MANAGER_BASE_A), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_ATM_MSG_T),                   INTENT(IN)    :: YDMSG
  REAL(KIND=JPRM_K), DIMENSION(:),      INTENT(IN)    :: VALUES_SP
END SUBROUTINE WRITE_ATM_SP_IF


!>
!> @brief Write fields contained in a request received by the IOserver.
!>
!> Reads and encodes all the fields present in a received request from the IOserver,
!> and subsequently writes the encoded data using the appropriate sinks.
!>
!> @param [inout] this  The object to be initialized.
!> @param [inout] ydmsg Message to be encoded
!>
SUBROUTINE WRITE_WAM_DP_IF( THIS, YDMSG, VALUES_DP )
  USE :: OM_CORE_MOD, ONLY: JPRD_K
  USE :: OM_CORE_MOD, ONLY: OM_WAM_MSG_T
  IMPORT :: OUTPUT_MANAGER_BASE_A
IMPLICIT NONE
  CLASS(OUTPUT_MANAGER_BASE_A), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_WAM_MSG_T),                   INTENT(IN)    :: YDMSG
  REAL(KIND=JPRD_K), DIMENSION(:),      INTENT(IN)    :: VALUES_DP
END SUBROUTINE WRITE_WAM_DP_IF


!>
!> @brief Write fields contained in a request received by the IOserver.
!>
!> Reads and encodes all the fields present in a received request from the IOserver,
!> and subsequently writes the encoded data using the appropriate sinks.
!>
!> @param [inout] this  The object to be initialized.
!> @param [inout] ydmsg Message to be encoded
!>
SUBROUTINE WRITE_WAM_SP_IF( THIS, YDMSG, VALUES_SP )
  USE :: OM_CORE_MOD, ONLY: JPRM_K
  USE :: OM_CORE_MOD, ONLY: OM_WAM_MSG_T
  IMPORT :: OUTPUT_MANAGER_BASE_A
IMPLICIT NONE
  CLASS(OUTPUT_MANAGER_BASE_A), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_WAM_MSG_T),                   INTENT(IN)    :: YDMSG
  REAL(KIND=JPRM_K), DIMENSION(:),      INTENT(IN)    :: VALUES_SP
END SUBROUTINE WRITE_WAM_SP_IF


!>
!> @brief Notify to the sinks the beginning of a new step started
!>
!> @param [inout] this  The object to be initialized.
!> @param [in]    kstep Step at which teh function has been called
!>
SUBROUTINE FLUSH_STEP_IF( THIS, KSTEP )
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  IMPORT :: OUTPUT_MANAGER_BASE_A
IMPLICIT NONE
  CLASS(OUTPUT_MANAGER_BASE_A), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: KSTEP
END SUBROUTINE FLUSH_STEP_IF


!>
!> @brief Notify to the sinks the the last step
!>
!> @param [inout] this  The object to be initialized.
!> @param [in]    kstep Step at which teh function has been called
!>
SUBROUTINE FLUSH_LAST_STEP_IF( THIS, KSTEP )
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  IMPORT :: OUTPUT_MANAGER_BASE_A
IMPLICIT NONE
  CLASS(OUTPUT_MANAGER_BASE_A), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: KSTEP
END SUBROUTINE FLUSH_LAST_STEP_IF


!>
!> @brief Notify to the sinks the beginning of a new step and the
!>        request to dump a checkpoint
!>
!> @param [inout] this  The object to be initialized.
!> @param [in]    kstep Step at which teh function has been called
!>
SUBROUTINE FLUSH_STEP_AND_TRIGGER_RESTART_IF( THIS, KSTEP )
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  IMPORT :: OUTPUT_MANAGER_BASE_A
IMPLICIT NONE
  CLASS(OUTPUT_MANAGER_BASE_A), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: KSTEP
END SUBROUTINE FLUSH_STEP_AND_TRIGGER_RESTART_IF


!>
!> @brief Finalisation of the output manager
!>
!> @param [inout] this  The object to be initialized.
!>
SUBROUTINE FINALISE_IF( THIS )
  IMPORT :: OUTPUT_MANAGER_BASE_A
IMPLICIT NONE
  CLASS(OUTPUT_MANAGER_BASE_A), INTENT(INOUT) :: THIS
END SUBROUTINE FINALISE_IF

END INTERFACE

! Whitelist of public symbols
PUBLIC :: OUTPUT_MANAGER_BASE_A

END MODULE OUTUPUT_MANAGER_BASE_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME