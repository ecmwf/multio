!> @file
!>
!> @brief Definition of the `OUTPUT_MANAGER_FACTORY_MOD` module.
!>
!> This module serves as a factory for various output managers,
!> providing a framework for their implementation.
!>
!> @author Mirco Valentini
!> @date January 12, 2024

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


! Definition of the module
#define PP_FILE_NAME 'output_manager_factory_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'OUTPUT_MANAGER_FACTORY_MOD'
MODULE OUTPUT_MANAGER_FACTORY_MOD

  ! Symbols imported from other modules within the project.
  USE :: OUTUPUT_MANAGER_BASE_MOD, ONLY: OUTPUT_MANAGER_BASE_A

IMPLICIT NONE

  ! Default visibility
  PRIVATE

  ! Whitelist of public symbols
  PUBLIC :: OUTPUT_MANAGER_BASE_A
  PUBLIC :: MAKE_OUTPUT_MANAGER
  PUBLIC :: DESTROY_OUTPUT_MANAGER

CONTAINS


!>
!> @brief Instantiates a new output manager.
!>
!> This function creates a new instance of an output manager, initializing it
!> with the provided IO server instance. After allocation, the 'setup' procedure
!> is automatically invoked with the IO server instance as an argument.
!>
!> @param [in]    OMTYPE         TYpe of the output manager to be created
!> @param [in]    PROCESSOR_TOPO Processor topology to be used in a multiprocessor run
!> @param [in]    MODEL_PARAMS   Model parameters that are frozen during the simulation
!> @param [in]    YAML_CFG_FNAME Name of the YAML main configuration file
!> @param [out]   OM             The newly created output manager.
!>
!> @note Ensure to call the setup procedure after instantiation for proper initialization.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAKE_OUTPUT_MANAGER'
SUBROUTINE MAKE_OUTPUT_MANAGER( OMTYPE, PROCESSOR_TOPO, MODEL_PARAMS, YAML_CFG_FNAME, OM )

  ! Symbols imported from other modules within the project.
  USE :: OUTUPUT_MANAGER_BASE_MOD, ONLY: OUTPUT_MANAGER_BASE_A
  USE :: OM_CORE_MOD,              ONLY: JPIB_K
  USE :: OM_CORE_MOD,              ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,              ONLY: MODEL_PAR_T
  USE :: NOOP_MOD,                 ONLY: NOOP_OUTPUT_MANAGER_T
  USE :: DUMP_MOD,                 ONLY: DUMP_OUTPUT_MANAGER_T
  USE :: GRIBX_BIN_MOD,            ONLY: GRIBX_BINARY_OUTPUT_MANAGER_T
  USE :: GRIBX2MULTIO_BIN_MOD,     ONLY: GRIBX2MULTIO_BIN_OUTPUT_MANAGER_T
  USE :: GRIBX2MULTIO_RAW_MOD,     ONLY: GRIBX2MULTIO_RAW_OUTPUT_MANAGER_T
  USE :: MULTIO_RAW_MOD,           ONLY: MULTIO_RAW_OUTPUT_MANAGER_T
  USE :: MULTIO_NO_ENC_MOD,        ONLY: MULTIO_NO_ENC_OUTPUT_MANAGER_T

  USE :: NOOP_MOD,                 ONLY: NOOP_OMNAME
  USE :: DUMP_MOD,                 ONLY: DUMP_OMNAME
  USE :: GRIBX_BIN_MOD,            ONLY: GRIBX_BINARY_OMNAME
  USE :: GRIBX2MULTIO_BIN_MOD,     ONLY: GRIBX2MULTIO_BIN_OMNAME
  USE :: GRIBX2MULTIO_RAW_MOD,     ONLY: GRIBX2MULTIO_RAW_OMNAME
  USE :: MULTIO_RAW_MOD,           ONLY: MULTIO_RAW_OMNAME
  USE :: MULTIO_NO_ENC_MOD,        ONLY: MULTIO_NO_ENC_OMNAME

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),                      INTENT(IN)  :: OMTYPE
  TYPE(PROC_TOPO_T),                     INTENT(IN)  :: PROCESSOR_TOPO
  TYPE(MODEL_PAR_T),                     INTENT(IN)  :: MODEL_PARAMS
  CHARACTER(LEN=*),                      INTENT(IN)  :: YAML_CFG_FNAME
  CLASS(OUTPUT_MANAGER_BASE_A), POINTER, INTENT(OUT) :: OM

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization
  STAT = 0_JPIB_K
  ERRMSG = ''
  NULLIFY(OM)


  !
  ! Allocate the requested IOmanager
  SELECT CASE(TRIM(ADJUSTL(OMTYPE)))

  ! ------------------------------------------------------------------------------------------------
  ! A no-op output manager. Used to demonstrate behavior without I/O
  ! when addressing complaints or testing scenarios.
  CASE ( 'NOOP', NOOP_OMNAME )
    ALLOCATE( NOOP_OUTPUT_MANAGER_T::OM, STAT=STAT, ERRMSG=ERRMSG )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )


  ! ------------------------------------------------------------------------------------------------
  ! A dump output manager. Used to dump all the data arrived to the IOserver
  CASE ( 'DUMP', DUMP_OMNAME )
    ALLOCATE( DUMP_OUTPUT_MANAGER_T::OM, STAT=STAT, ERRMSG=ERRMSG )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )


  ! ------------------------------------------------------------------------------------------------
  ! Grib[1|2] output manager utilizing grib functionalities as a sink,
  ! designed to operate independently of multIO.
  CASE ( 'GRIBX', GRIBX_BINARY_OMNAME )
    ALLOCATE( GRIBX_BINARY_OUTPUT_MANAGER_T::OM, STAT=STAT, ERRMSG=ERRMSG )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )


  ! ------------------------------------------------------------------------------------------------
  ! Grib[1|2] output manager leveraging multIO as a sink,
  ! designed for saving binary-encoded grib files through the multIO API.
  CASE ( 'GRIBX2MULTIO_BINARY', GRIBX2MULTIO_BIN_OMNAME )
    ALLOCATE( GRIBX2MULTIO_BIN_OUTPUT_MANAGER_T::OM, STAT=STAT, ERRMSG=ERRMSG )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )


  ! ------------------------------------------------------------------------------------------------
  ! Grib[1|2] output manager utilizing multIO as a sink. Binary-encoded
  ! grib files are transformed into multIO metadata and a raw data array.
  ! The metadata and data are subsequently handed to the multIO API for
  ! on-the-fly post-processing and storage to disk.
  CASE ( 'GRIBX2MULTIO_RAW', GRIBX2MULTIO_RAW_OMNAME )
    ALLOCATE( GRIBX2MULTIO_RAW_OUTPUT_MANAGER_T::OM, STAT=STAT, ERRMSG=ERRMSG )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )


  ! ------------------------------------------------------------------------------------------------
  ! Pure multIO output manager constructing MultIO metadata directly in the encoders, bypassing
  ! grib eccodes api. The encoding logic is still used. Metadata and data are then passed to the
  ! multIO API for on-the-fly post-processing and storage to disk.
  CASE ( 'MULTIO_RAW', MULTIO_RAW_OMNAME )
    ALLOCATE( MULTIO_RAW_OUTPUT_MANAGER_T::OM, STAT=STAT, ERRMSG=ERRMSG )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )

  ! ------------------------------------------------------------------------------------------------
  ! Pure multIO output manager constructing metadata directly from io_server requests,
  ! bypassing the grib/eccodes API. The metadata are directly the raw information arriving from the
  ! model. No encoding logic at all in this output manager. Metadata and data are then passed to the
  ! multIO API for on-the-fly post-processing and storage to disk.
  ! The idea is to use this to test funtionalities of the new MultIO IOserver
  CASE ( 'MULTIO_NO_ENC', MULTIO_NO_ENC_OMNAME )
    ALLOCATE( MULTIO_NO_ENC_OUTPUT_MANAGER_T::OM, STAT=STAT, ERRMSG=ERRMSG )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )


  ! ------------------------------------------------------------------------------------------------
  CASE DEFAULT
    PP_DEBUG_DEVELOP_THROW( 100 )
  END SELECT

  ! Initialize the object
  CALL OM%SETUP( YAML_CFG_FNAME, PROCESSOR_TOPO, MODEL_PARAMS )

  ! Deallocate the error message if allocated
  IF ( ALLOCATED(ERRMSG) ) THEN
    DEALLOCATE(ERRMSG)
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

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'error allocating ('//TRIM(ADJUSTL(OMTYPE))//') -> '//TRIM(ADJUSTL(ERRMSG)) )
      DEALLOCATE(ERRMSG)

    CASE (100)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown output manager: "'//TRIM(ADJUSTL(OMTYPE))//'"' )

    CASE (101)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error message allocated at the end of the function: ['//TRIM(ADJUSTL(ERRMSG))//']' )

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

END SUBROUTINE MAKE_OUTPUT_MANAGER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Destroys an output manager.
!>
!> This function deallocates the specified output manager and invokes its finalization
!> procedure, passing the associated IO server instance as an argument.
!>
!> @param [inout] om    The pointer to the output manager to be destroyed
!>
!> @note Before deallocation, the finalization procedure is called for proper cleanup.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'DESTROY_OUTPUT_MANAGER'
SUBROUTINE DESTROY_OUTPUT_MANAGER( OM )

  ! Symbols imported from other modules within the project.
  USE :: OUTUPUT_MANAGER_BASE_MOD, ONLY: OUTPUT_MANAGER_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(OUTPUT_MANAGER_BASE_A), POINTER, INTENT(INOUT) :: OM

  ! Local variables
  INTEGER :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization
  STAT = 0
  ERRMSG = ''

  ! Check allocation status of the output manager
  PP_DEBUG_DEVELOP_COND_THROW( .NOT. ASSOCIATED(OM), 1 )

  ! Finalise the object
  CALL OM%FINALISE( )

  ! Free memmory
  DEALLOCATE( OM, STAT=STAT, ERRMSG=ERRMSG )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )

  ! Reset the pointer to null
  NULLIFY(OM)

  ! Check error message
  IF ( ALLOCATED(ERRMSG) ) THEN
    DEALLOCATE(ERRMSG)
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

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'output manager not allocated' )

    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'failed to deallocate object -> '//TRIM(ADJUSTL(ERRMSG)) )
      DEALLOCATE(ERRMSG)

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

END SUBROUTINE DESTROY_OUTPUT_MANAGER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE OUTPUT_MANAGER_FACTORY_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
