!> @file
!!
!! @brief Constants and enumerators used in the multio interface.
!!
!! This file contains a collection of constants and enumerators that are utilized
!! within the multio interface.
!!

module multio_api_constants_mod

    use, intrinsic :: iso_c_binding,   only: c_ptr
    use, intrinsic :: iso_c_binding,   only: c_null_ptr

implicit none

    ! Default visibility
    private

    ! Error codes
    integer, parameter :: MULTIO_SUCCESS = 0
    integer, parameter :: MULTIO_ERROR_ECKIT_EXCEPTION = 1
    integer, parameter :: MULTIO_ERROR_GENERAL_EXCEPTION = 2
    integer, parameter :: MULTIO_ERROR_UNKNOWN_EXCEPTION = 3


    ! Error handling definitions
    type :: multio_failure_info
        type(c_ptr) :: impl = c_null_ptr
    end type


    ! Whitelist of public symbols
    public :: multio_failure_info
    public :: MULTIO_SUCCESS
    public :: MULTIO_ERROR_ECKIT_EXCEPTION
    public :: MULTIO_ERROR_GENERAL_EXCEPTION
    public :: MULTIO_ERROR_UNKNOWN_EXCEPTION

end module multio_api_constants_mod
