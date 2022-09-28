module multio_api

    use, intrinsic :: iso_c_binding
    implicit none

    ! Error values

    integer, public, parameter :: MULTIO_SUCCESS = 0
    integer, public, parameter :: MULTIO_ERROR_ECKIT_EXCEPTION = 1
    integer, public, parameter :: MULTIO_ERROR_GENERAL_EXCEPTION = 2
    integer, public, parameter :: MULTIO_ERROR_UNKNOWN_EXCEPTION = 3

    private

    integer, parameter :: dp = selected_real_kind(15, 307)
    integer, parameter :: double_size = 8 !c_sizeof(1.0_dp) !intel compiler...
    integer, parameter :: int64 = selected_int_kind(15)


    type multio_configurationcontext
        type(c_ptr) :: impl = c_null_ptr
    contains
        procedure :: new => multio_new_configurationcontext
        procedure :: new_from_filename => multio_new_configurationcontext_from_filename
        procedure :: delete => multio_delete_configurationcontext
        procedure :: set_path => multio_conf_set_path
        procedure :: mpi_allow_world_default_comm => multio_conf_mpi_allow_world_default_comm
        procedure :: mpi_client_id => multio_conf_mpi_client_id
        procedure :: mpi_parent_comm => multio_conf_mpi_parent_comm
        procedure :: mpi_return_client_comm => multio_conf_mpi_return_client_comm
        procedure :: mpi_return_server_comm => multio_conf_mpi_return_server_comm
        procedure :: mpi_split_client_color => multio_conf_mpi_split_client_color
        procedure :: mpi_split_server_color => multio_conf_mpi_split_server_color
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
        procedure :: write_mask => multio_write_mask
        procedure :: write_field => multio_write_field
        procedure :: field_is_active => multio_field_is_active
        procedure :: category_is_fully_active => multio_category_is_fully_active
    end type

    type multio_metadata
        type(c_ptr) :: impl = c_null_ptr
    contains
        procedure :: new => multio_new_metadata
        procedure :: delete => multio_delete_metadata
        procedure :: set_int_value => multio_metadata_set_int_value
        procedure :: set_long_value => multio_metadata_set_long_value
        procedure :: set_longlong_value => multio_metadata_set_longlong_value
        procedure :: set_string_value => multio_metadata_set_string_value
        procedure :: set_bool_value => multio_metadata_set_bool_value
        procedure :: set_float_value => multio_metadata_set_float_value
        procedure :: set_double_value => multio_metadata_set_double_value
    end type

    ! Type declarations

    public :: multio_configurationcontext
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

    interface
        pure function strlen(str) result(len) &
                bind(c)
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: str
            integer(c_int) :: len
        end function
    end interface

    ! Wrap the C api functions

    interface

        function c_multio_version(pstr) result(err) &
                bind(c, name='multio_version')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(out) :: pstr
            integer(c_int) :: err
        end function

        function c_multio_vcs_version(pstr) result(err) &
                bind(c, name='multio_vcs_version')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(out) :: pstr
            integer(c_int) :: err
        end function

        function multio_initialise() result(err) &
                bind(c, name='multio_initialise')
            use, intrinsic :: iso_c_binding
            implicit none
            integer(c_int) :: err
        end function

        function c_multio_start_server(cc) result(err) &
                bind(c, name='multio_start_server')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: cc
            integer(c_int) :: err
        end function
        
        function c_multio_set_failure_handler(handler, context) result(err) &
                bind(c, name='multio_set_failure_handler')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_funptr), intent(in), value :: handler
            type(c_ptr), intent(in), value :: context
            integer(c_int) :: err
        end function

        function c_multio_error_string(err) result(error_string) &
                bind(c, name='multio_error_string')
            use, intrinsic :: iso_c_binding
            implicit none
            integer(c_int), intent(in), value :: err
            type(c_ptr) :: error_string
        end function
        
        ! Configuration context api
        function c_multio_new_configurationcontext(cc) result(err) &
                bind(c, name='multio_new_configurationcontext')
                use, intrinsic :: iso_c_binding
                implicit none
                type(c_ptr), intent(out) :: cc
                integer(c_int) :: err
        end function
        
        function c_multio_new_configurationcontext_from_filename(cc, file_name) result(err) &
                bind(c, name='multio_new_configurationcontext_from_filename')
                use, intrinsic :: iso_c_binding
                implicit none
                type(c_ptr), intent(in), value :: file_name
                type(c_ptr), intent(out) :: cc
                integer(c_int) :: err
        end function
        
        function c_multio_delete_configurationcontext(cc) result(err) &
                bind(c, name='multio_delete_configurationcontext')
                use, intrinsic :: iso_c_binding
                implicit none
                type(c_ptr), intent(in), value :: cc
                integer(c_int) :: err
        end function
        
        function c_multio_conf_set_path(cc, path) result(err) &
                bind(c, name='multio_conf_set_path')
                use, intrinsic :: iso_c_binding
                implicit none
                type(c_ptr), intent(in), value :: path
                type(c_ptr), intent(in), value :: cc
                integer(c_int) :: err
        end function
        
        function c_multio_conf_mpi_allow_world_default_comm(cc, allow) result(err) &
                bind(c, name='multio_conf_mpi_allow_world_default_comm')
                use, intrinsic :: iso_c_binding
                implicit none
                logical(c_bool), intent(in), value :: allow
                type(c_ptr), intent(in), value :: cc
                integer(c_int) :: err
        end function
        
        function c_multio_conf_mpi_client_id(cc, client_id) result(err) &
                bind(c, name='multio_conf_mpi_client_id')
                use, intrinsic :: iso_c_binding
                implicit none
                type(c_ptr), intent(in), value :: client_id 
                type(c_ptr), intent(in), value :: cc
                integer(c_int) :: err
        end function
        
        function c_multio_conf_mpi_parent_comm(cc, parent_comm) result(err) &
                bind(c, name='multio_conf_mpi_parent_comm')
                use, intrinsic :: iso_c_binding
                implicit none
                integer(c_int), intent(in), value :: parent_comm
                type(c_ptr), intent(in), value :: cc
                integer(c_int) :: err
        end function
        
        function c_multio_conf_mpi_return_client_comm(cc, return_comm) result(err) &
                bind(c, name='multio_conf_mpi_return_client_comm')
                use, intrinsic :: iso_c_binding
                implicit none
                integer(c_int), intent(out) :: return_comm ! can be c_null_ptr
                type(c_ptr), intent(in), value :: cc
                integer(c_int) :: err
        end function
        
        function c_multio_conf_mpi_return_server_comm(cc, return_comm) result(err) &
                bind(c, name='multio_conf_mpi_return_server_comm')
                use, intrinsic :: iso_c_binding
                implicit none
                integer(c_int), intent(out) :: return_comm ! can be c_null_ptr
                type(c_ptr), intent(in), value :: cc
                integer(c_int) :: err
        end function
        
        function c_multio_conf_mpi_split_client_color(cc, color) result(err) &
                bind(c, name='multio_conf_mpi_split_client_color')
                use, intrinsic :: iso_c_binding
                implicit none
                integer(c_int), intent(in), value :: color
                type(c_ptr), intent(in), value :: cc
                integer(c_int) :: err
        end function
        
        function c_multio_conf_mpi_split_server_color(cc, color) result(err) &
                bind(c, name='multio_conf_mpi_split_server_color')
                use, intrinsic :: iso_c_binding
                implicit none
                integer(c_int), intent(in), value :: color
                type(c_ptr), intent(in), value :: cc
                integer(c_int) :: err
        end function
        

        ! Handle object api

        function c_multio_new_handle(handle, cc) result(err) &
                bind(c, name='multio_new_handle')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(out) :: handle
            type(c_ptr), intent(in), value :: cc
            integer(c_int) :: err
        end function

        function c_multio_delete_handle(handle) result(err) &
                bind(c, name='multio_delete_handle')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: handle
            integer(c_int) :: err
        end function

        function c_multio_open_connections(handle) result(err) &
                bind(c, name='multio_open_connections')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: handle
            integer(c_int) :: err
        end function

        function c_multio_close_connections(handle) result(err) &
                bind(c, name='multio_close_connections')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: handle
            integer(c_int) :: err
        end function

        function c_multio_write_step_complete(handle, metadata) result(err) &
                bind(c, name='multio_write_step_complete')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: handle
            type(c_ptr), intent(in), value :: metadata
            integer(c_int) :: err
        end function

        function c_multio_write_domain(handle, metadata, data, size) result(err) &
                bind(c, name='multio_write_domain')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: handle
            type(c_ptr), intent(in), value :: metadata
            integer(c_int), dimension(*), intent(in) :: data
            integer(c_int), intent(in), value :: size
            integer(c_int) :: err
        end function

        function c_multio_write_mask(handle, metadata, data, size) result(err) &
                bind(c, name='multio_write_mask')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: handle
            type(c_ptr), intent(in), value :: metadata
            real(c_double), dimension(*), intent(in) :: data
            integer(c_int), intent(in), value :: size
            integer(c_int) :: err
        end function

        function c_multio_write_field(handle, metadata, data, size) result(err) &
                bind(c, name='multio_write_field')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: handle
            type(c_ptr), intent(in), value :: metadata
            real(c_double), dimension(*), intent(in) :: data
            integer(c_int), intent(in), value :: size
            integer(c_int) :: err
        end function
        
        function c_multio_field_is_active(handle, field, set_value) result(err) &
                bind(c, name='multio_field_is_active')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: handle
            type(c_ptr), intent(in), value :: field
            logical(c_bool), intent(out) :: set_value
            integer(c_int) :: err
        end function

        function c_multio_category_is_fully_active(handle, category, set_value) result(err) &
                bind(c, name='multio_category_is_fully_active')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: handle
            type(c_ptr), intent(in), value :: category
            logical(c_bool), intent(out) :: set_value
            integer(c_int) :: err
        end function

        ! Metadata object api

        function c_multio_new_metadata(metadata) result(err) &
                bind(c, name='multio_new_metadata')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(out) :: metadata
            integer(c_int) :: err
        end function

        function c_multio_delete_metadata(metadata) result(err) &
                bind(c, name='multio_delete_metadata')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: metadata
            integer(c_int) :: err
        end function

        function c_multio_metadata_set_int_value(metadata, key, value) result(err) &
                bind(c, name='multio_metadata_set_int_value')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: metadata
            type(c_ptr), intent(in), value :: key
            integer(c_int), intent(in), value:: value
            integer(c_int) :: err
        end function

        function c_multio_metadata_set_long_value(metadata, key, value) result(err) &
                bind(c, name='multio_metadata_set_long_value')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: metadata
            type(c_ptr), intent(in), value :: key
            integer(c_long), intent(in), value:: value
            integer(c_int) :: err
        end function

        function c_multio_metadata_set_longlong_value(metadata, key, value) result(err) &
                bind(c, name='multio_metadata_set_longlong_value')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: metadata
            type(c_ptr), intent(in), value :: key
            integer(c_long_long), intent(in), value:: value
            integer(c_int) :: err
        end function

        function c_multio_metadata_set_bool_value(metadata, key, value) result(err) &
                bind(c, name='multio_metadata_set_bool_value')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: metadata
            type(c_ptr), intent(in), value :: key
            logical(c_bool), intent(in), value:: value
            integer(c_int) :: err
        end function

        function c_multio_metadata_set_float_value(metadata, key, value) result(err) &
                bind(c, name='multio_metadata_set_float_value')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: metadata
            type(c_ptr), intent(in), value :: key
            real(c_float), intent(in), value:: value
            integer(c_int) :: err
        end function

        function c_multio_metadata_set_double_value(metadata, key, value) result(err) &
                bind(c, name='multio_metadata_set_double_value')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: metadata
            type(c_ptr), intent(in), value :: key
            real(c_double), intent(in), value:: value
            integer(c_int) :: err
        end function

        function c_multio_metadata_set_string_value(metadata, key, value) result(err) &
                bind(c, name='multio_metadata_set_string_value')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: metadata
            type(c_ptr), intent(in), value :: key
            type(c_ptr), intent(in), value :: value
            integer(c_int) :: err
        end function

    end interface

contains

    function fortranise_cstr(cstr) result(fstr)
        type(c_ptr), intent(in) :: cstr
        character(:), allocatable, target :: fstr
        character(c_char), pointer :: tmp(:)
        integer :: length

        length = strlen(cstr)
        allocate(character(length) :: fstr)
        call c_f_pointer(cstr, tmp, [length])
        fstr = transfer(tmp(1:length), fstr)
    end function

    subroutine failure_handler_wrapper(unused_context, error) &
                bind(c)
        type(c_ptr), value :: unused_context
        integer(c_long), intent(in), value :: error
        call failure_handler_fn(failure_handler_context, int(error))
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

        failure_handler_fn => handler
        failure_handler_context = context
        err = c_multio_set_failure_handler(c_funloc(failure_handler_wrapper), c_null_ptr)
    end function

    function multio_version(version_str) result(err)
        character(:), allocatable, intent(out) :: version_str
        type(c_ptr) :: tmp_str
        integer :: err
        err = c_multio_version(tmp_str)
        if (err == MULTIO_SUCCESS) version_str = fortranise_cstr(tmp_str)
    end function

    function multio_vcs_version(git_sha1) result(err)
        character(:), allocatable, intent(out) :: git_sha1
        type(c_ptr) :: tmp_str
        integer :: err
        err = c_multio_vcs_version(tmp_str)
        if (err == MULTIO_SUCCESS) git_sha1 = fortranise_cstr(tmp_str)
    end function

    function multio_error_string(err) result(error_string)
        integer, intent(in) :: err
        character(:), allocatable, target :: error_string
        error_string = fortranise_cstr(c_multio_error_string(err))
    end function
    
    function multio_start_server(cc) result(err)
        class(multio_configurationcontext), intent(in) :: cc
        integer :: err
        err = c_multio_start_server(cc%impl)
    end function
    
    ! Methods for configuration context objects
    
    function multio_new_configurationcontext(cc) result(err) 
            class(multio_configurationcontext), intent(out) :: cc
            integer :: err
            err = c_multio_new_configurationcontext(cc%impl)
    end function
    
    function multio_new_configurationcontext_from_filename(cc, file_name) result(err) 
            class(multio_configurationcontext), intent(inout) :: cc
            integer :: err
            character(*), intent(in) :: file_name
            character(:), allocatable, target :: nullified_path
            nullified_path = trim(file_name) // c_null_char
            err = c_multio_new_configurationcontext_from_filename(cc%impl, c_loc(nullified_path))
    end function
    
    function multio_delete_configurationcontext(cc) result(err) 
            class(multio_configurationcontext), intent(inout) :: cc
            integer :: err
            err = c_multio_delete_configurationcontext(cc%impl)
            cc%impl = c_null_ptr
    end function
    
    function multio_conf_set_path(cc, path) result(err) 
            class(multio_configurationcontext), intent(inout) :: cc
            integer :: err
            character(*), intent(in) :: path
            character(:), allocatable, target :: nullified_path
            nullified_path = trim(path) // c_null_char
            err = c_multio_conf_set_path(cc%impl, c_loc(nullified_path))
    end function
    
    function multio_conf_mpi_allow_world_default_comm(cc, allow) result(err) 
            class(multio_configurationcontext), intent(inout) :: cc
            logical(c_bool), intent(in), value :: allow
            integer :: err
            err = c_multio_conf_mpi_allow_world_default_comm(cc%impl, allow)
    end function
    
    function multio_conf_mpi_client_id(cc, client_id) result(err) 
            class(multio_configurationcontext), intent(inout) :: cc
            integer :: err
            character(*), intent(in) :: client_id
            character(:), allocatable, target :: nullified_client_id
            nullified_client_id = trim(client_id) // c_null_char
            err = c_multio_conf_mpi_client_id(cc%impl, c_loc(nullified_client_id))
    end function
    
    function multio_conf_mpi_parent_comm(cc, parent_comm) result(err) 
            class(multio_configurationcontext), intent(inout) :: cc
            integer(c_int), intent(in), value :: parent_comm
            integer :: err
            err = c_multio_conf_mpi_parent_comm(cc%impl, parent_comm)
    end function
    
    function multio_conf_mpi_return_client_comm(cc, return_comm) result(err) 
            class(multio_configurationcontext), intent(inout) :: cc
            integer(c_int), intent(out) :: return_comm ! can be c_null_ptr
            integer :: err
            err = c_multio_conf_mpi_return_client_comm(cc%impl, return_comm)
    end function
    
    function multio_conf_mpi_return_server_comm(cc, return_comm) result(err) 
            class(multio_configurationcontext), intent(inout) :: cc
            integer(c_int), intent(out) :: return_comm ! can be c_null_ptr
            integer :: err
            err = c_multio_conf_mpi_return_server_comm(cc%impl, return_comm)
    end function
    
    function multio_conf_mpi_split_client_color(cc, color) result(err) 
            class(multio_configurationcontext), intent(inout) :: cc
            integer(c_int), intent(in), value :: color
            integer :: err
            err = c_multio_conf_mpi_split_client_color(cc%impl, color)
    end function
    
    function multio_conf_mpi_split_server_color(cc, color) result(err) 
            class(multio_configurationcontext), intent(inout) :: cc
            integer(c_int), intent(in), value :: color
            integer :: err
            err = c_multio_conf_mpi_split_server_color(cc%impl, color)
    end function

    ! Methods for handle objects

    function multio_new_handle(handle, cc) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_configurationcontext), intent(in) :: cc
        integer :: err
        err = c_multio_new_handle(handle%impl, cc%impl)
    end function

    function multio_delete_handle(handle) result(err)
        class(multio_handle), intent(inout) :: handle
        integer :: err
        err = c_multio_delete_handle(handle%impl)
        handle%impl = c_null_ptr
    end function

    function multio_open_connections(handle) result(err)
        class(multio_handle), intent(inout) :: handle
        integer :: err
        err = c_multio_open_connections(handle%impl)
    end function

    function multio_close_connections(handle) result(err)
        class(multio_handle), intent(inout) :: handle
        integer :: err
        err = c_multio_close_connections(handle%impl)
    end function

    function multio_write_step_complete(handle, metadata) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        integer :: err
        err = c_multio_write_step_complete(handle%impl, metadata%impl)
    end function

    function multio_write_domain(handle, metadata, data, size) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        integer :: err
        integer, dimension(*), intent(in) :: data
        integer, intent(in), value :: size
        err = c_multio_write_domain(handle%impl, metadata%impl, data, size)
    end function

    function multio_write_mask(handle, metadata, data, size) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        integer :: err

        real(dp), dimension(*), intent(in) :: data
        integer, intent(in), value :: size
        err = c_multio_write_mask(handle%impl, metadata%impl, data, size)
    end function

    function multio_write_field(handle, metadata, data, size) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        integer :: err

        real(dp), dimension(*), intent(in) :: data
        integer, intent(in), value :: size
        err = c_multio_write_field(handle%impl, metadata%impl, data, size)
    end function
    
    function multio_field_is_active(handle, field, set_value) result(err)
        class(multio_handle), intent(inout) :: handle
        logical(c_bool), intent(out) :: set_value
        character(*), intent(in) :: field
        integer :: err
        character(:), allocatable, target :: nullified_field
        nullified_field = trim(field) // c_null_char
        err = c_multio_field_is_active(handle%impl, c_loc(nullified_field), set_value)
    end function
    
    function multio_category_is_fully_active(handle, category, set_value) result(err)
        class(multio_handle), intent(inout) :: handle
        logical(c_bool), intent(out) :: set_value
        character(*), intent(in) :: category
        integer :: err
        character(:), allocatable, target :: nullified_category
        nullified_category = trim(category) // c_null_char
        err = c_multio_category_is_fully_active(handle%impl, c_loc(nullified_category), set_value)
    end function

    ! Methods for metadata objects
    function multio_new_metadata(metadata) result(err)
        class(multio_metadata), intent(inout) :: metadata
        integer :: err
        err = c_multio_new_metadata(metadata%impl)
    end function

    function multio_delete_metadata(metadata) result(err)
        class(multio_metadata), intent(inout) :: metadata
        integer :: err
        err = c_multio_delete_metadata(metadata%impl)
        metadata%impl = c_null_ptr
    end function

    function multio_metadata_set_int_value(metadata, key, value) result(err)
        class(multio_metadata), intent(in) :: metadata
        character(*), intent(in) :: key
        integer(c_int), intent(in), value :: value
        integer :: err

        character(:), allocatable, target :: nullified_key
        nullified_key = trim(key) // c_null_char

        err = c_multio_metadata_set_int_value(metadata%impl, c_loc(nullified_key), value)
    end function

    function multio_metadata_set_long_value(metadata, key, value) result(err)
        class(multio_metadata), intent(in) :: metadata
        character(*), intent(in) :: key
        integer(c_long), intent(in), value :: value
        integer :: err

        character(:), allocatable, target :: nullified_key
        nullified_key = trim(key) // c_null_char

        err = c_multio_metadata_set_long_value(metadata%impl, c_loc(nullified_key), value)
    end function

    function multio_metadata_set_longlong_value(metadata, key, value) result(err)
        class(multio_metadata), intent(in) :: metadata
        character(*), intent(in) :: key
        integer(c_long_long), intent(in), value :: value
        integer :: err

        character(:), allocatable, target :: nullified_key
        nullified_key = trim(key) // c_null_char

        err = c_multio_metadata_set_longlong_value(metadata%impl, c_loc(nullified_key), value)
    end function

    function multio_metadata_set_float_value(metadata, key, value) result(err)
        class(multio_metadata), intent(in) :: metadata
        character(*), intent(in) :: key
        real(c_float), intent(in), value :: value
        integer :: err

        character(:), allocatable, target :: nullified_key
        nullified_key = trim(key) // c_null_char

        err = c_multio_metadata_set_float_value(metadata%impl, c_loc(nullified_key), value)
    end function

    function multio_metadata_set_double_value(metadata, key, value) result(err)
        class(multio_metadata), intent(in) :: metadata
        character(*), intent(in) :: key
        real(dp), intent(in), value :: value
        integer :: err

        character(:), allocatable, target :: nullified_key
        nullified_key = trim(key) // c_null_char

        err = c_multio_metadata_set_double_value(metadata%impl, c_loc(nullified_key), value)
    end function

    function multio_metadata_set_bool_value(metadata, key, value) result(err)
        class(multio_metadata), intent(in) :: metadata
        character(*), intent(in) :: key
        logical(c_bool), intent(in), value :: value
        integer :: err

        character(:), allocatable, target :: nullified_key
        nullified_key = trim(key) // c_null_char

        err = c_multio_metadata_set_bool_value(metadata%impl, c_loc(nullified_key), value)
    end function

    function multio_metadata_set_string_value(metadata, key, value) result(err)
        class(multio_metadata), intent(in) :: metadata
        character(*), intent(in) :: key
        character(*), intent(in) :: value
        integer :: err
        character(:), allocatable, target :: nullified_key
        character(:), allocatable, target :: nullified_value

        nullified_key = trim(key) // c_null_char
        nullified_value = trim(value) // c_null_char

        err = c_multio_metadata_set_string_value(metadata%impl, c_loc(nullified_key), c_loc(nullified_value))
    end function

end module multio_api
