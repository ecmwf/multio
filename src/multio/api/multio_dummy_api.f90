module multio_dummy_api

    use, intrinsic :: iso_c_binding
    implicit none

    ! Error values

    integer, public, parameter :: MULTIO_SUCCESS = 0
    integer, public, parameter :: MULTIO_ERROR_ECKIT_EXCEPTION = 1
    integer, public, parameter :: MULTIO_ERROR_GENERAL_EXCEPTION = 2
    integer, public, parameter :: MULTIO_ERROR_UNKNOWN_EXCEPTION = 3

    private

    integer, parameter :: sp = selected_real_kind(6, 37)
    integer, parameter :: dp = selected_real_kind(15, 307)
    integer, parameter :: double_size = 8 !c_sizeof(1.0_dp) !intel compiler...
    integer, parameter :: float_size = 4 !c_sizeof(1.0_dp) !intel compiler...
    integer, parameter :: int64 = selected_int_kind(15)


    type multio_configuration
        type(c_ptr) :: impl = c_null_ptr
    contains
        procedure :: new => multio_new_configuration
        procedure :: new_from_filename => multio_new_configuration_from_filename
        procedure :: delete => multio_delete_configuration
        procedure :: set_path => multio_conf_set_path
        procedure :: mpi_allow_world_default_comm => multio_conf_mpi_allow_world_default_comm
        procedure :: mpi_client_id => multio_conf_mpi_client_id
        procedure :: mpi_parent_comm => multio_conf_mpi_parent_comm
        procedure :: mpi_return_client_comm => multio_conf_mpi_return_client_comm
        procedure :: mpi_return_server_comm => multio_conf_mpi_return_server_comm
    end type

    type multio_handle
        type(c_ptr) :: impl = c_null_ptr
    contains
        procedure :: new => multio_new_handle
        procedure :: delete => multio_delete_handle
        procedure :: open_connections => multio_open_connections
        procedure :: close_connections => multio_close_connections
        procedure :: write_step_complete => multio_write_step_complete
        procedure :: write_domain => multio_write_domain
        procedure :: write_mask_float_1d => multio_write_mask_float_1d
        procedure :: write_mask_double_1d => multio_write_mask_double_1d
        procedure :: write_mask_float_2d => multio_write_mask_float_2d
        procedure :: write_mask_double_2d => multio_write_mask_double_2d
        generic   :: write_mask => write_mask_float_1d, write_mask_float_2d, write_mask_double_1d, write_mask_double_2d
        procedure :: write_field_float_1d => multio_write_field_float_1d
        procedure :: write_field_double_1d => multio_write_field_double_1d
        procedure :: write_field_float_2d => multio_write_field_float_2d
        procedure :: write_field_double_2d => multio_write_field_double_2d
        generic   :: write_field => write_field_float_1d, write_field_float_2d, write_field_double_1d, write_field_double_2d
        procedure :: field_accepted => multio_field_accepted
        ! procedure :: field_is_active => multio_field_is_active
        ! procedure :: category_is_fully_active => multio_category_is_fully_active
    end type

    type multio_metadata
        type(c_ptr) :: impl = c_null_ptr
    contains
        procedure :: new => multio_new_metadata
        procedure :: delete => multio_delete_metadata
        procedure :: set_int => multio_metadata_set_int
        procedure :: set_long => multio_metadata_set_long
        procedure :: set_longlong => multio_metadata_set_longlong
        procedure :: set_string => multio_metadata_set_string
        procedure :: set_bool => multio_metadata_set_bool
        procedure :: set_float => multio_metadata_set_float
        procedure :: set_double => multio_metadata_set_double
        ! Not possible to overload integers with the same dimensions
        generic   :: set => set_int, set_string, set_bool, set_float, set_double
    end type

    ! Type declarations

    public :: multio_configuration
    public :: multio_handle
    public :: multio_metadata

    ! Configuration management functions

    public :: multio_initialise
    public :: multio_version, multio_vcs_version
    public :: multio_set_failure_handler
    public :: multio_start_server
    public :: multio_error_string

    ! Error handling definitions

    abstract interface
        subroutine failure_handler_t(context, error)
            implicit none
            integer, parameter :: int64 = selected_int_kind(15)
            integer(int64), intent(inout) :: context
            integer, intent(in) :: error
        end subroutine
    end interface

    integer(int64), save :: failure_handler_context
    procedure(failure_handler_t), pointer, save :: failure_handler_fn

    ! For utility

contains

    function multio_initialise() result(err)
        implicit none
        integer(c_int) :: err
        err = 0
    end function

    subroutine multio_failure_handler_wrapper(unused_context, error)
        type(c_ptr), value :: unused_context
        integer(c_long), intent(in), value :: error
    end subroutine

    function multio_set_failure_handler(handler, context) result(err)

        integer(int64) :: context
        integer :: err

        interface
            subroutine handler (ctx, err)
                implicit none
                integer, parameter :: int64 = selected_int_kind(15)
                integer(int64), intent(inout) :: ctx
                integer, intent(in) :: err
            end subroutine
        end interface

        err = 0
    end function

    function multio_version(version_str) result(err)
        character(:), allocatable, intent(out) :: version_str
        integer :: err
        err = 0
        version_str='dummy_interface'
    end function

    function multio_vcs_version(git_sha1) result(err)
        character(:), allocatable, intent(out) :: git_sha1
        type(c_ptr) :: tmp_str
        integer :: err
        err = 0
        git_sha1='dummy_sha'
    end function

    function multio_error_string(err) result(error_string)
        integer, intent(in) :: err
        character(:), allocatable, target :: error_string
        error_string = 'This is just a dummy string'
    end function

    function multio_start_server(cc) result(err)
        class(multio_configuration), intent(in) :: cc
        integer :: err
        err = 0
    end function

    ! Methods for configuration context objects

    function multio_new_configuration(cc) result(err)
            class(multio_configuration), intent(out) :: cc
            integer :: err
            err = 0
    end function

    function multio_new_configuration_from_filename(cc, file_name) result(err)
            class(multio_configuration), intent(inout) :: cc
            character(*), intent(in) :: file_name
            integer :: err
            err = 0
    end function

    function multio_delete_configuration(cc) result(err)
            class(multio_configuration), intent(inout) :: cc
            integer :: err
            err = 0
    end function

    function multio_conf_set_path(cc, path) result(err)
            class(multio_configuration), intent(inout) :: cc
            integer :: err
            character(*), intent(in) :: path
            err = 0
    end function

    function multio_conf_mpi_allow_world_default_comm(cc, allow) result(err)
            class(multio_configuration), intent(inout) :: cc
            logical(c_bool), intent(in), value :: allow
            integer :: err
            err = 0
    end function

    function multio_conf_mpi_client_id(cc, client_id) result(err)
            class(multio_configuration), intent(inout) :: cc
            character(*), intent(in) :: client_id
            integer :: err
            err = 0
    end function

    function multio_conf_mpi_parent_comm(cc, parent_comm) result(err)
            class(multio_configuration), intent(inout) :: cc
            integer(c_int), intent(in), value :: parent_comm
            integer :: err
            err = 0
    end function

    function multio_conf_mpi_return_client_comm(cc, return_comm) result(err)
            class(multio_configuration), intent(inout) :: cc
            integer(c_int), intent(out) :: return_comm ! can be c_null_ptr
            integer :: err
            err = 0
    end function

    function multio_conf_mpi_return_server_comm(cc, return_comm) result(err)
            class(multio_configuration), intent(inout) :: cc
            integer(c_int), intent(out) :: return_comm ! can be c_null_ptr
            integer :: err
            err = 0
    end function

    ! Methods for handle objects

    function multio_new_handle(handle, cc) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_configuration), intent(in) :: cc
        integer :: err
        err = 0
    end function

    function multio_delete_handle(handle) result(err)
        class(multio_handle), intent(inout) :: handle
        integer :: err
        err = 0
    end function

    function multio_open_connections(handle) result(err)
        class(multio_handle), intent(inout) :: handle
        integer :: err
        err = 0
    end function

    function multio_close_connections(handle) result(err)
        class(multio_handle), intent(inout) :: handle
        integer :: err
        err = 0
    end function

    function multio_write_step_complete(handle, metadata) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        integer :: err
        err = 0
    end function

    function multio_write_domain(handle, metadata, data) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        integer, dimension(:), intent(in) :: data
        integer :: err
        err = 0
    end function

    function multio_write_mask_float_1d(handle, metadata, data) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        real(sp), dimension(:), target, intent(in) :: data
        integer :: err
        err = 0
    end function

    function multio_write_mask_float_2d(handle, metadata, data) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        real(sp), dimension(:,:), target, intent(in) :: data
        integer :: err
        err = 0
    end function

    function multio_write_mask_double_1d(handle, metadata, data) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        real(dp), dimension(:), target, intent(in) :: data
        integer :: err
        err = 0
    end function

    function multio_write_mask_double_2d(handle, metadata, data) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        real(dp), dimension(:,:), target, intent(in) :: data
        integer :: err
        err = 0
    end function

    function multio_write_field_float_1d(handle, metadata, data) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        real(sp), dimension(:), target, intent(in) :: data
        integer :: err
        err = 0
    end function

    function multio_write_field_float_2d(handle, metadata, data) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        real(sp), dimension(:,:), target, intent(in) :: data
        integer :: err
        err = 0
    end function

    function multio_write_field_double_1d(handle, metadata, data) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        real(dp), dimension(:), target, intent(in) :: data
        integer :: err
        err = 0
    end function

    function multio_write_field_double_2d(handle, metadata, data) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        real(dp), dimension(:,:), target, intent(in) :: data
        integer :: err
        err = 0
    end function

    function multio_field_accepted(handle, metadata, set_value) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        logical(c_bool), intent(out) :: set_value
        integer :: err
        err = 0
    end function

    ! Methods for metadata objects
    function multio_new_metadata(metadata) result(err)
        class(multio_metadata), intent(inout) :: metadata
        integer :: err
        err = 0
    end function

    function multio_delete_metadata(metadata) result(err)
        class(multio_metadata), intent(inout) :: metadata
        integer :: err
        err = 0
        metadata%impl = c_null_ptr
    end function

    function multio_metadata_set_int(metadata, key, value) result(err)
        class(multio_metadata), intent(in) :: metadata
        character(*), intent(in) :: key
        integer(c_int), intent(in), value :: value
        integer :: err
        err = 0
    end function

    function multio_metadata_set_long(metadata, key, value) result(err)
        class(multio_metadata), intent(in) :: metadata
        character(*), intent(in) :: key
        integer(c_long), intent(in), value :: value
        integer :: err
        err = 0
    end function

    function multio_metadata_set_longlong(metadata, key, value) result(err)
        class(multio_metadata), intent(in) :: metadata
        character(*), intent(in) :: key
        integer(c_long_long), intent(in), value :: value
        integer :: err
        err = 0
    end function

    function multio_metadata_set_float(metadata, key, value) result(err)
        class(multio_metadata), intent(in) :: metadata
        character(*), intent(in) :: key
        real(c_float), intent(in), value :: value
        integer :: err
        err = 0
    end function

    function multio_metadata_set_double(metadata, key, value) result(err)
        class(multio_metadata), intent(in) :: metadata
        character(*), intent(in) :: key
        real(dp), intent(in), value :: value
        integer :: err
        err = 0
    end function

    function multio_metadata_set_bool(metadata, key, value) result(err)
        class(multio_metadata), intent(in) :: metadata
        character(*), intent(in) :: key
        logical(c_bool), intent(in), value :: value
        integer :: err
        err = 0
    end function

    function multio_metadata_set_string(metadata, key, value) result(err)
        class(multio_metadata), intent(in) :: metadata
        character(*), intent(in) :: key
        character(*), intent(in) :: value
        integer :: err
        err = 0
    end function

end module multio_dummy_api
