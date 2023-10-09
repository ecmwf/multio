!> @file
!!
!! @brief Constants and enumerators used in the multio interface.
!!
!! This file contains a collection of constants and enumerators that are utilized
!! within the multio interface.
!!

module multio_api_constants_mod
implicit none

    ! Default visibility
    private

    ! Error codes
    integer, parameter :: MULTIO_SUCCESS = 0
    integer, parameter :: MULTIO_ERROR_ECKIT_EXCEPTION = 1
    integer, parameter :: MULTIO_ERROR_GENERAL_EXCEPTION = 2
    integer, parameter :: MULTIO_ERROR_UNKNOWN_EXCEPTION = 3


    ! Whitelist of public symbols
    public :: MULTIO_SUCCESS
    public :: MULTIO_ERROR_ECKIT_EXCEPTION
    public :: MULTIO_ERROR_GENERAL_EXCEPTION
    public :: MULTIO_ERROR_UNKNOWN_EXCEPTION

end module multio_api_constants_mod
