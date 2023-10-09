!> @file
!!
!! @brief Comprehensive List of Exposed Symbols in the "multio" Fortran Interface
!!
!! This file serves as a comprehensive catalog of symbols made available through the Fortran interface of the "multio" library.
!! These symbols encompass a range of functions, classes, and entities that enable efficient data routing from distributed meteorological and earth-system models.
!!
!! The "multio" library provides essential features, including I/O multiplexing for generating multiple output data streams from a single input,
!! I/O server functionality to aggregate horizontal fields from distributed parallel models, and post-processing pipelines for tasks like deriving meteorological products and grid interpolation.
!!
!! At the core of the data routing process lies the integration of metadata with the data itself, coupled with a configurable setup that defines post-processing pipelines.
!!
!! By leveraging the capabilities offered by the "multio" Fortran interface, users can streamline data management, optimize routing efficiency, and enhance the flexibility of meteorological and earth-system modeling.
!!

module multio_api

    ! Constants
    use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    use :: multio_api_constants_mod, only: MULTIO_ERROR_ECKIT_EXCEPTION
    use :: multio_api_constants_mod, only: MULTIO_ERROR_GENERAL_EXCEPTION
    use :: multio_api_constants_mod, only: MULTIO_ERROR_UNKNOWN_EXCEPTION

    ! Datatypes
    use :: multio_api_configuration_mod, only: multio_configuration
    use :: multio_api_handle_mod,        only: multio_handle
    use :: multio_api_metadata_mod,      only: multio_metadata
    use :: multio_api_data_mod,          only: multio_data

    ! Utils
    use :: multio_api_utils_mod, only: multio_initialise
    use :: multio_api_utils_mod, only: multio_start_server
    use :: multio_api_utils_mod, only: multio_version
    use :: multio_api_utils_mod, only: multio_vcs_version
    use :: multio_api_utils_mod, only: multio_error_string

    ! Failure handling
    use :: multio_api_error_handling_mod, only: multio_failure_info

implicit none

    ! Default symbol visibility
    private

    !! White list of the visible symbols

    ! Public constants
    public :: MULTIO_SUCCESS
    public :: MULTIO_ERROR_ECKIT_EXCEPTION
    public :: MULTIO_ERROR_GENERAL_EXCEPTION
    public :: MULTIO_ERROR_UNKNOWN_EXCEPTION

    ! Public datatypes
    public :: multio_configuration
    public :: multio_handle
    public :: multio_metadata
    public :: multio_data

    ! Public utils
    public :: multio_initialise
    public :: multio_start_server
    public :: multio_version
    public :: multio_vcs_version
    public :: multio_error_string

    ! Public error handling
    public :: multio_failure_info

end module multio_api
