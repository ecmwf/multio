!> @file
!!
!! @brief Set of functions for the general management of the multio interface.
!!

module multio_api_utils_mod
implicit none

    ! Default visibility
    private

    ! Whitelist of public symbols
    public :: multio_initialise
    public :: multio_start_server
    public :: multio_version
    public :: multio_vcs_version
    public :: multio_error_string

contains


    !> @brief Initialize multio.
    !!
    !! This function initializes the multio interface.
    !!
    !! @return An error code indicating the operation's success.
    !!
    function multio_initialise() result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interfaces
        interface
            function c_multio_initialise() result(err) &
                    bind(c, name='multio_initialise')
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                integer(c_int) :: err
            end function c_multio_initialise
        end interface
        ! Implementation
        c_err = c_multio_initialise()
        ! Output cast and cleanup
        err = int(c_err,kind(err))
#else
#warning "Dummy API is enabled"
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_initialise


    !> @brief Start a multio server.
    !!
    !! This function starts a multio server.
    !!
    !! @param [in] cc The configuration object with the configuration of the server.
    !!
    !! @return An error code indicating the operation's success.
    !!
    function multio_start_server(cc) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding,     only: c_int
        ! Variable references from the project
        use :: multio_api_configuration_mod, only: multio_configuration
        use :: multio_api_constants_mod,     only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_configuration), intent(inout) :: cc
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interfaces
        interface
            function c_multio_start_server(cc) result(err) &
                bind(c, name='multio_start_server')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: cc
                integer(c_int) :: err
            end function c_multio_start_server
        end interface
        ! Implementation
        c_err = c_multio_start_server(cc%c_ptr())
        ! Cast output values
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_start_server


    !> @brief Convert a string from c-style to fortran-style
    !!
    !! This function removes converts a c-string to a fortran-string
    !!
    !! @param [in,out] cstr  The c string to be converted
    !!
    !! @return The fortran string
    !!
    function fortranise_cstr(cstr) result(fstr)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_ptr
        use, intrinsic :: iso_c_binding, only: c_char
        use, intrinsic :: iso_c_binding, only: c_f_pointer
    implicit none
        ! Dummy arguments
        type(c_ptr), intent(in) :: cstr
        ! Function result
        character(:), allocatable, target :: fstr
        ! Local variables
        character(c_char), dimension(:), pointer :: tmp
        integer :: length
        ! Private interfaces
        interface
            pure function strlen( str ) result(len) &
                    bind(c,name='strlen')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: str
                integer(kind=c_int) :: len
            end function strlen
        end interface
        ! Private inerfaces
        length = strlen(cstr)
        allocate(character(length) :: fstr)
        call c_f_pointer(cstr, tmp, [length])
        fstr = transfer(tmp(1:length), fstr)
        ! Exit point
        return
    end function fortranise_cstr


    !> @brief Return the version of multio.
    !!
    !! This function returns the version of the multio library.
    !!
    !! @param [out] version_str The string containing the multio version.
    !!
    !! @return An error code indicating the operation's success.
    !!
    function multio_version(version_str) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_ptr
        use, intrinsic :: iso_c_binding, only: c_int
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        character(:), allocatable, intent(out) :: version_str
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        type(c_ptr) :: tmp_str
        integer(kind=c_int) :: c_err
        ! Private interface
        interface
            function c_multio_version(pstr) result(err) &
                    bind(c, name='multio_version')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), intent(out) :: pstr
                integer(c_int) :: err
            end function
        end interface
        ! Implementation
        c_err = c_multio_version(tmp_str)
        if (c_err == MULTIO_SUCCESS) version_str = fortranise_cstr(tmp_str)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_version


    !> @brief Return the Git SHA of multio.
    !!
    !! This function returns the Git SHA of the multio library.
    !!
    !! @param [out] git_sha1 The Git SHA string of multio.
    !!
    !! @return An error code indicating the operation's success.
    !!
    function multio_vcs_version(git_sha1) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_ptr
        use, intrinsic :: iso_c_binding, only: c_int
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        character(:), allocatable, intent(out) :: git_sha1
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        type(c_ptr) :: tmp_str
        integer(kind=c_int) :: c_err
        ! Private interfaces
        interface
            function c_multio_vcs_version(pstr) result(err) &
                    bind(c, name='multio_vcs_version')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), intent(out) :: pstr
                integer(c_int) :: err
            end function c_multio_vcs_version
        end interface
        ! Implementation
        c_err = c_multio_vcs_version(tmp_str)
        if (c_err == MULTIO_SUCCESS) git_sha1 = fortranise_cstr(tmp_str)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_vcs_version


    !> @brief Return the version of multio.
    !!
    !! This function returns the version of the multio library.
    !!
    !! @param [in] err  The error code.
    !! @param [in] info The error info.
    !!
    !! @return The error string associated with the provided error code.
    !!
    function multio_error_string(err, info) result(error_string)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding,      only: c_int
        ! Variable references from the project
        use :: multio_api_error_handling_mod, only: multio_failure_info
        use :: multio_api_constants_mod,      only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        integer,                              intent(in) :: err
        class(multio_failure_info), optional, intent(in) :: info
        ! Function result
        character(:), allocatable, target :: error_string
#if !defined(MULTIO_DUMMY_API)
        ! Local variablees
        integer(kind=c_int) :: c_err
        ! Private interfaces
        interface
            function c_multio_error_string_info(err, info) result(error_string) &
                bind(c, name='multio_error_string_info')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                integer(c_int), value, intent(in) :: err
                type(c_ptr),    value, intent(in) :: info
                type(c_ptr) :: error_string
            end function c_multio_error_string_info
            function c_multio_error_string(err) result(error_string) &
                    bind(c, name='multio_error_string')
                use, intrinsic :: iso_c_binding, only: c_int
                use, intrinsic :: iso_c_binding, only: c_ptr
            implicit none
                integer(c_int), value, intent(in) :: err
                type(c_ptr) :: error_string
            end function c_multio_error_string
        end interface
        ! Implementation
        c_err = int(err,c_int)
        if (present(info)) then
            error_string = fortranise_cstr(c_multio_error_string_info(c_err,info%impl))
        else
            error_string = fortranise_cstr(c_multio_error_string(c_err))
        end if
#else
        error_string = 'MULTIO DUMMY API IS ENABLED'
#endif
        ! Exit point
        return
    end function multio_error_string

end module multio_api_utils_mod