module multio_api

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
        integer(c_int), pointer :: failure_id => null()
    contains
        procedure :: new_default => multio_new_configuration
        procedure :: new_from_filename => multio_new_configuration_from_filename
        generic   :: new => new_default, new_from_filename
        procedure :: delete => multio_delete_configuration
        procedure :: set_failure_handler => multio_config_set_failure_handler
        procedure :: set_path => multio_conf_set_path
        procedure, private :: mpi_allow_world_default_comm_cbool   => multio_conf_mpi_allow_world_default_comm_cbool
        procedure, private :: mpi_allow_world_default_comm_logical => multio_conf_mpi_allow_world_default_comm_logical
        generic   :: mpi_allow_world_default_comm => mpi_allow_world_default_comm_cbool, &
                                                   & mpi_allow_world_default_comm_logical
        procedure :: mpi_parent_comm => multio_conf_mpi_parent_comm
        procedure :: mpi_return_client_comm => multio_conf_mpi_return_client_comm
        procedure :: mpi_return_server_comm => multio_conf_mpi_return_server_comm
    end type

    type multio_handle
        type(c_ptr) :: impl = c_null_ptr
        integer(c_int), pointer :: failure_id => null()
    contains
        procedure :: new_handle => multio_new_handle
        procedure :: new_handle_default => multio_new_handle_default
        generic :: new => new_handle, new_handle_default
        procedure :: delete => multio_delete_handle
        procedure :: set_failure_handler => multio_handle_set_failure_handler
        procedure :: open_connections => multio_open_connections
        procedure :: close_connections => multio_close_connections
        procedure :: flush => multio_flush
        procedure :: notify => multio_notify
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

    type multio_failure_info
        type(c_ptr) :: impl = c_null_ptr
    end type

    ! Type declarations

    public :: multio_configuration
    public :: multio_handle
    public :: multio_metadata
    public :: multio_failure_info

    ! Configuration management functions

    public :: multio_initialise
    public :: multio_version, multio_vcs_version
    public :: multio_start_server
    public :: multio_error_string

    ! Error handling definitions

    abstract interface
        subroutine failure_handler_t(context, error, info)
            import multio_failure_info
            implicit none
            integer, parameter :: int64 = selected_int_kind(15)
            integer(int64), intent(inout) :: context
            integer, intent(in) :: error
            class(multio_failure_info), intent(in) :: info
        end subroutine
    end interface

    type multio_fort_failure_info_node
        integer(c_int) :: id = 0

        integer(int64) :: context = 0

        procedure(failure_handler_t), nopass, pointer :: handler_fn => null()


        TYPE(multio_fort_failure_info_node), pointer :: next => null()
    end type

    type multio_fort_failure_info_list
        integer(c_int) :: lastId = 0
        integer :: count = 0
        type(multio_fort_failure_info_node), pointer :: head => null()
        type(multio_fort_failure_info_node), pointer :: tail => null()
    contains
        procedure :: callHandler => multio_fort_failure_call
        procedure :: add => multio_fort_failure_add
        procedure :: remove => multio_fort_failure_remove
    end type


    type(multio_fort_failure_info_list), save :: failure_info_list
    ! integer(int64), save :: failure_handler_context
    ! procedure(failure_handler_t), pointer, save :: failure_handler_fn

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

        function c_multio_error_string_info(err, info) result(error_string) &
                bind(c, name='multio_error_string_info')
            use, intrinsic :: iso_c_binding
            implicit none
            integer(c_int), intent(in), value :: err
            type(c_ptr), intent(in), value :: info
            type(c_ptr) :: error_string
        end function

        function c_multio_error_string(err) result(error_string) &
                bind(c, name='multio_error_string')
            use, intrinsic :: iso_c_binding
            implicit none
            integer(c_int), intent(in), value :: err
            type(c_ptr) :: error_string
        end function

        ! Configuration api
        function c_multio_new_configuration(cc) result(err) &
                bind(c, name='multio_new_configuration')
                use, intrinsic :: iso_c_binding
                implicit none
                type(c_ptr), intent(out) :: cc
                integer(c_int) :: err
        end function

        function c_multio_new_configuration_from_filename(cc, file_name) result(err) &
                bind(c, name='multio_new_configuration_from_filename')
                use, intrinsic :: iso_c_binding
                implicit none
                type(c_ptr), intent(in), value :: file_name
                type(c_ptr), intent(out) :: cc
                integer(c_int) :: err
        end function

        function c_multio_delete_configuration(cc) result(err) &
                bind(c, name='multio_delete_configuration')
                use, intrinsic :: iso_c_binding
                implicit none
                type(c_ptr), intent(in), value :: cc
                integer(c_int) :: err
        end function

        function c_multio_config_set_failure_handler(cc, handler, context) result(err) &
                bind(c, name='multio_config_set_failure_handler')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: cc
            type(c_funptr), intent(in), value :: handler
            type(c_ptr), intent(in), value :: context
            integer(c_int) :: err
        end function

        function c_multio_handle_set_failure_handler(mio, handler, context) result(err) &
                bind(c, name='multio_handle_set_failure_handler')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: mio
            type(c_funptr), intent(in), value :: handler
            type(c_ptr), intent(in), value :: context
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


        ! Handle object api

        function c_multio_new_handle(handle, cc) result(err) &
                bind(c, name='multio_new_handle')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(out) :: handle
            type(c_ptr), intent(in), value :: cc
            integer(c_int) :: err
        end function

        function c_multio_new_handle_default(handle) result(err) &
                bind(c, name='multio_new_handle_default')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(out) :: handle
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

        function c_multio_flush(handle, metadata) result(err) &
                bind(c, name='multio_flush')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: handle
            type(c_ptr), intent(in), value :: metadata
            integer(c_int) :: err
        end function

        function c_multio_notify(handle, metadata) result(err) &
                bind(c, name='multio_notify')
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

        function c_multio_write_mask_float(handle, metadata, data, size) result(err) &
                bind(c, name='multio_write_mask_float')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: handle
            type(c_ptr), intent(in), value :: metadata
            ! real(c_float), dimension(*), intent(in) :: data
            type(c_ptr), intent(in), value :: data
            integer(c_int), intent(in), value :: size
            integer(c_int) :: err
        end function

        function c_multio_write_mask_double(handle, metadata, data, size) result(err) &
                bind(c, name='multio_write_mask_double')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: handle
            type(c_ptr), intent(in), value :: metadata
            ! real(c_double), dimension(*), intent(in) :: data
            type(c_ptr), intent(in), value :: data
            integer(c_int), intent(in), value :: size
            integer(c_int) :: err
        end function

        function c_multio_write_field_float(handle, metadata, data, size) result(err) &
                bind(c, name='multio_write_field_float')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: handle
            type(c_ptr), intent(in), value :: metadata
            type(c_ptr), intent(in), value :: data
            integer(c_int), intent(in), value :: size
            integer(c_int) :: err
        end function

        function c_multio_write_field_double(handle, metadata, data, size) result(err) &
                bind(c, name='multio_write_field_double')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: handle
            type(c_ptr), intent(in), value :: metadata
            type(c_ptr), intent(in), value :: data
            integer(c_int), intent(in), value :: size
            integer(c_int) :: err
        end function

        function c_multio_field_accepted(handle, metadata, set_value) result(err) &
                bind(c, name='multio_field_accepted')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: handle
            type(c_ptr), intent(in), value :: metadata
            logical(c_bool), intent(out) :: set_value
            integer(c_int) :: err
        end function

        ! Metadata object api

        function c_multio_new_metadata(metadata, handle) result(err) &
                bind(c, name='multio_new_metadata')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(out) :: metadata
            type(c_ptr), intent(in), value :: handle
            integer(c_int) :: err
        end function

        function c_multio_delete_metadata(metadata) result(err) &
                bind(c, name='multio_delete_metadata')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: metadata
            integer(c_int) :: err
        end function

        function c_multio_metadata_set_int(metadata, key, value) result(err) &
                bind(c, name='multio_metadata_set_int')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: metadata
            type(c_ptr), intent(in), value :: key
            integer(c_int), intent(in), value:: value
            integer(c_int) :: err
        end function

        function c_multio_metadata_set_long(metadata, key, value) result(err) &
                bind(c, name='multio_metadata_set_long')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: metadata
            type(c_ptr), intent(in), value :: key
            integer(c_long), intent(in), value:: value
            integer(c_int) :: err
        end function

        function c_multio_metadata_set_longlong(metadata, key, value) result(err) &
                bind(c, name='multio_metadata_set_longlong')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: metadata
            type(c_ptr), intent(in), value :: key
            integer(c_long_long), intent(in), value:: value
            integer(c_int) :: err
        end function

        function c_multio_metadata_set_bool(metadata, key, value) result(err) &
                bind(c, name='multio_metadata_set_bool')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: metadata
            type(c_ptr), intent(in), value :: key
            logical(c_bool), intent(in), value:: value
            integer(c_int) :: err
        end function

        function c_multio_metadata_set_float(metadata, key, value) result(err) &
                bind(c, name='multio_metadata_set_float')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: metadata
            type(c_ptr), intent(in), value :: key
            real(c_float), intent(in), value:: value
            integer(c_int) :: err
        end function

        function c_multio_metadata_set_double(metadata, key, value) result(err) &
                bind(c, name='multio_metadata_set_double')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: metadata
            type(c_ptr), intent(in), value :: key
            real(c_double), intent(in), value:: value
            integer(c_int) :: err
        end function

        function c_multio_metadata_set_string(metadata, key, value) result(err) &
                bind(c, name='multio_metadata_set_string')
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

    subroutine failure_handler_wrapper(context_id, error, info) &
                bind(c)
        type(c_ptr), value :: context_id
        integer(c_int), intent(in), value :: error
        type(c_ptr), intent(in), value :: info

        type(multio_failure_info) :: finfo

        integer(c_int), pointer :: id
        call c_f_pointer( context_id, id )

        finfo%impl = info

        call failure_info_list%callHandler(id, int(error), finfo)
    end subroutine

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

    function multio_error_string(err, info) result(error_string)
        integer, intent(in) :: err
        class(multio_failure_info), optional, intent(in) :: info
        character(:), allocatable, target :: error_string
        if(present(info)) then
            error_string = fortranise_cstr(c_multio_error_string_info(err, info%impl))
        else
            error_string = fortranise_cstr(c_multio_error_string(err))
        end if
    end function

    function multio_start_server(cc) result(err)
        class(multio_configuration), intent(in) :: cc
        integer :: err
        err = c_multio_start_server(cc%impl)
    end function

    ! Methods for configuration context objects

    function multio_new_configuration(cc) result(err)
            class(multio_configuration), intent(out) :: cc
            integer :: err
            err = c_multio_new_configuration(cc%impl)
    end function

    function multio_new_configuration_from_filename(cc, file_name) result(err)
            class(multio_configuration), intent(inout) :: cc
            integer :: err
            character(*), intent(in) :: file_name
            character(:), allocatable, target :: nullified_path
            nullified_path = trim(file_name) // c_null_char
            err = c_multio_new_configuration_from_filename(cc%impl, c_loc(nullified_path))
    end function

    function multio_delete_configuration(cc) result(err)
        class(multio_configuration), intent(inout) :: cc
        integer :: err
        err = c_multio_delete_configuration(cc%impl)
        cc%impl = c_null_ptr

        if(ASSOCIATED(cc%failure_id)) then
           call failure_info_list%remove(cc%failure_id)
           cc%failure_id => null()
        end if
    end function

    subroutine multio_fort_failure_call(ffi, id, err, info)
       class(multio_fort_failure_info_list), intent(inout) :: ffi
       integer(c_int), intent(in) :: id
       integer :: err
       class(multio_failure_info), intent(in) :: info

       type(multio_fort_failure_info_node), pointer :: node

       node => ffi%head

       do while(ASSOCIATED(node))
         if (node%id == id) then
           call node%handler_fn(node%context, err, info)
           node => null()
         else
           node => node%next
         end if
       end do
    end subroutine


    function multio_fort_failure_add(ffi, handler_fn, context) result(new_id_loc)
        class(multio_fort_failure_info_list), intent(inout) :: ffi
        procedure(failure_handler_t), pointer :: handler_fn
        integer(int64) :: context

        type(c_ptr) :: new_id_loc
        class(multio_fort_failure_info_node), pointer :: new_node

        ffi%lastId = ffi%lastId + 1
        ffi%count = ffi%count + 1

        allocate(new_node);
        new_node%id = ffi%lastId
        new_node%handler_fn => handler_fn
        new_node%context = context

        new_id_loc = c_loc(new_node%id)

        if(.not. ASSOCIATED(ffi%head)) then
            ffi%head => new_node
        end if

        if(ASSOCIATED(ffi%tail)) then
            ffi%tail%next => new_node
        endif
        ffi%tail => new_node
    end function

    subroutine multio_fort_failure_remove(ffi, id)
       class(multio_fort_failure_info_list), intent(inout) :: ffi
       integer(c_int), intent(in) :: id

       type(multio_fort_failure_info_node), pointer :: node
       type(multio_fort_failure_info_node), pointer :: node_prev

       node_prev => null()
       node => ffi%head

       do while (ASSOCIATED(node))
         if (node%id == id) then
           if(ASSOCIATED(node_prev)) then
             node_prev%next => node%next
           end if

           if(ASSOCIATED(ffi%head, node)) then
             ffi%head => node%next
           endif

           if(ASSOCIATED(ffi%tail, node)) then
             ffi%tail => node_prev
           endif

           ffi%count = ffi%count - 1

           deallocate(node)
           node => null()
         else
           node_prev => node
           node => node%next
         end if
       end do
    end subroutine

    function multio_config_set_failure_handler(cc, handler, context) result(err)
        class(multio_configuration), intent(inout) :: cc
        integer(int64) :: context
        integer :: err

        interface
            subroutine handler (ctx, err, info)
                import multio_failure_info
                implicit none
                integer, parameter :: int64 = selected_int_kind(15)
                integer(int64), intent(inout) :: ctx
                integer, intent(in) :: err
                class(multio_failure_info), intent(in) :: info
            end subroutine
        end interface

        type(c_ptr) :: new_id_loc
        integer(c_int), pointer :: old_id => null()
        procedure(failure_handler_t), pointer :: handler_fn

        handler_fn => handler

        if(ASSOCIATED(cc%failure_id)) then
            old_id => cc%failure_id
        end if

        new_id_loc = failure_info_list%add(handler_fn, context)
        call c_f_pointer(new_id_loc, cc%failure_id)
        err = c_multio_config_set_failure_handler(cc%impl, c_funloc(failure_handler_wrapper), new_id_loc)

        if(ASSOCIATED(old_id)) then
            call failure_info_list%remove(old_id)
        end if
    end function

    function multio_handle_set_failure_handler(mio, handler, context) result(err)
        class(multio_handle), intent(inout) :: mio
        integer(int64) :: context
        integer :: err

        interface
            subroutine handler (ctx, err, info)
                import multio_failure_info
                implicit none
                integer, parameter :: int64 = selected_int_kind(15)
                integer(int64), intent(inout) :: ctx
                integer, intent(in) :: err
                class(multio_failure_info), intent(in) :: info
            end subroutine
        end interface

        type(c_ptr) :: new_id_loc
        integer(c_int), pointer :: old_id => null()
        procedure(failure_handler_t), pointer :: handler_fn

        handler_fn => handler

        if(ASSOCIATED(mio%failure_id)) then
            old_id => mio%failure_id
        end if

        new_id_loc = failure_info_list%add(handler_fn, context)
        call c_f_pointer(new_id_loc, mio%failure_id)
        err = c_multio_handle_set_failure_handler(mio%impl, c_funloc(failure_handler_wrapper), new_id_loc)

        if(ASSOCIATED(old_id)) then
            call failure_info_list%remove(old_id)
        end if
    end function

    function multio_conf_set_path(cc, path) result(err)
            class(multio_configuration), intent(inout) :: cc
            integer :: err
            character(*), intent(in) :: path
            character(:), allocatable, target :: nullified_path
            nullified_path = trim(path) // c_null_char
            err = c_multio_conf_set_path(cc%impl, c_loc(nullified_path))
    end function

    function multio_conf_mpi_allow_world_default_comm_cbool(cc, allow) result(err)
            class(multio_configuration), intent(inout) :: cc
            logical(c_bool), intent(in), value :: allow
            integer :: err

            err = c_multio_conf_mpi_allow_world_default_comm(cc%impl, allow)
    end function

   function multio_conf_mpi_allow_world_default_comm_logical(cc, allow) result(err)
            class(multio_configuration), intent(inout) :: cc
            logical, intent(in), value :: allow
            integer :: err

            err = c_multio_conf_mpi_allow_world_default_comm(cc%impl, logical(allow,c_bool))
    end function

    function multio_conf_mpi_parent_comm(cc, parent_comm) result(err)
            class(multio_configuration), intent(inout) :: cc
            integer(c_int), intent(in), value :: parent_comm
            integer :: err
            err = c_multio_conf_mpi_parent_comm(cc%impl, parent_comm)
    end function

    function multio_conf_mpi_return_client_comm(cc, return_comm) result(err)
            class(multio_configuration), intent(inout) :: cc
            integer(c_int), intent(out) :: return_comm ! can be c_null_ptr
            integer :: err
            err = c_multio_conf_mpi_return_client_comm(cc%impl, return_comm)
    end function

    function multio_conf_mpi_return_server_comm(cc, return_comm) result(err)
            class(multio_configuration), intent(inout) :: cc
            integer(c_int), intent(out) :: return_comm ! can be c_null_ptr
            integer :: err
            err = c_multio_conf_mpi_return_server_comm(cc%impl, return_comm)
    end function

    ! Methods for handle objects

    function multio_new_handle(handle, cc) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_configuration), intent(inout) :: cc
        integer :: err
        handle%failure_id => cc%failure_id
        cc%failure_id => null()
        err = c_multio_new_handle(handle%impl, cc%impl)
    end function

    function multio_new_handle_default(handle) result(err)
        class(multio_handle), intent(inout) :: handle
        integer :: err
        err = c_multio_new_handle_default(handle%impl)
    end function

    function multio_delete_handle(handle) result(err)
        class(multio_handle), intent(inout) :: handle
        integer :: err
        err = c_multio_delete_handle(handle%impl)
        handle%impl = c_null_ptr

        if(ASSOCIATED(handle%failure_id)) then
           call failure_info_list%remove(handle%failure_id)
           handle%failure_id => null()
        end if
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

    function multio_flush(handle, metadata) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        integer :: err
        err = c_multio_flush(handle%impl, metadata%impl)
    end function

    function multio_notify(handle, metadata) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        integer :: err
        err = c_multio_notify(handle%impl, metadata%impl)
    end function

    function multio_write_domain(handle, metadata, data) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        integer :: err
        integer, dimension(:), intent(in) :: data
        err = c_multio_write_domain(handle%impl, metadata%impl, data, size(data))
    end function

    function multio_write_mask_float_1d(handle, metadata, data) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        integer :: err

        real(sp), dimension(:), target, intent(in) :: data
        err = c_multio_write_mask_float(handle%impl, metadata%impl, c_loc(data), size(data))
    end function

    function multio_write_mask_float_2d(handle, metadata, data) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        integer :: err

        real(sp), dimension(:,:), target, intent(in) :: data
        err = c_multio_write_mask_float(handle%impl, metadata%impl, c_loc(data), size(data))
    end function

    function multio_write_mask_double_1d(handle, metadata, data) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        integer :: err

        real(dp), dimension(:), target, intent(in) :: data
        err = c_multio_write_mask_double(handle%impl, metadata%impl, c_loc(data), size(data))
    end function

    function multio_write_mask_double_2d(handle, metadata, data) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        integer :: err

        real(dp), dimension(:,:), target, intent(in) :: data
        err = c_multio_write_mask_double(handle%impl, metadata%impl, c_loc(data), size(data))
    end function

    function multio_write_field_float_1d(handle, metadata, data) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        integer :: err

        real(sp), dimension(:), target, intent(in) :: data
        err = c_multio_write_field_float(handle%impl, metadata%impl, c_loc(data), size(data))
    end function

    function multio_write_field_float_2d(handle, metadata, data) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        integer :: err

        real(sp), dimension(:,:), target, intent(in) :: data
        err = c_multio_write_field_float(handle%impl, metadata%impl, c_loc(data), size(data))
    end function

    function multio_write_field_double_1d(handle, metadata, data) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        integer :: err

        real(dp), dimension(:), target, intent(in) :: data
        err = c_multio_write_field_double(handle%impl, metadata%impl, c_loc(data), size(data))
    end function

    function multio_write_field_double_2d(handle, metadata, data) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        integer :: err

        real(dp), dimension(:,:), target, intent(in) :: data
        err = c_multio_write_field_double(handle%impl, metadata%impl, c_loc(data), size(data))
    end function

    function multio_field_accepted(handle, metadata, set_value) result(err)
        class(multio_handle), intent(inout) :: handle
        class(multio_metadata), intent(in) :: metadata
        logical(c_bool), intent(out) :: set_value
        integer :: err
        character(:), allocatable, target :: nullified_field

        err = c_multio_field_accepted(handle%impl, metadata%impl, set_value)
    end function

    ! Methods for metadata objects
    function multio_new_metadata(metadata, handle) result(err)
        class(multio_metadata), intent(inout) :: metadata
        class(multio_handle), intent(in) :: handle
        integer :: err
        err = c_multio_new_metadata(metadata%impl, handle%impl)
    end function

    function multio_delete_metadata(metadata) result(err)
        class(multio_metadata), intent(inout) :: metadata
        integer :: err
        err = c_multio_delete_metadata(metadata%impl)
        metadata%impl = c_null_ptr
    end function

    function multio_metadata_set_int(metadata, key, value) result(err)
        class(multio_metadata), intent(in) :: metadata
        character(*), intent(in) :: key
        integer(c_int), intent(in), value :: value
        integer :: err

        character(:), allocatable, target :: nullified_key
        nullified_key = trim(key) // c_null_char

        err = c_multio_metadata_set_int(metadata%impl, c_loc(nullified_key), value)
    end function

    function multio_metadata_set_long(metadata, key, value) result(err)
        class(multio_metadata), intent(in) :: metadata
        character(*), intent(in) :: key
        integer(c_long), intent(in), value :: value
        integer :: err

        character(:), allocatable, target :: nullified_key
        nullified_key = trim(key) // c_null_char

        err = c_multio_metadata_set_long(metadata%impl, c_loc(nullified_key), value)
    end function

    function multio_metadata_set_longlong(metadata, key, value) result(err)
        class(multio_metadata), intent(in) :: metadata
        character(*), intent(in) :: key
        integer(c_long_long), intent(in), value :: value
        integer :: err

        character(:), allocatable, target :: nullified_key
        nullified_key = trim(key) // c_null_char

        err = c_multio_metadata_set_longlong(metadata%impl, c_loc(nullified_key), value)
    end function

    function multio_metadata_set_float(metadata, key, value) result(err)
        class(multio_metadata), intent(in) :: metadata
        character(*), intent(in) :: key
        real(c_float), intent(in), value :: value
        integer :: err

        character(:), allocatable, target :: nullified_key
        nullified_key = trim(key) // c_null_char

        err = c_multio_metadata_set_float(metadata%impl, c_loc(nullified_key), value)
    end function

    function multio_metadata_set_double(metadata, key, value) result(err)
        class(multio_metadata), intent(in) :: metadata
        character(*), intent(in) :: key
        real(dp), intent(in), value :: value
        integer :: err

        character(:), allocatable, target :: nullified_key
        nullified_key = trim(key) // c_null_char

        err = c_multio_metadata_set_double(metadata%impl, c_loc(nullified_key), value)
    end function

    function multio_metadata_set_bool(metadata, key, value) result(err)
        class(multio_metadata), intent(in) :: metadata
        character(*), intent(in) :: key
        logical(c_bool), intent(in), value :: value
        integer :: err

        character(:), allocatable, target :: nullified_key
        nullified_key = trim(key) // c_null_char

        err = c_multio_metadata_set_bool(metadata%impl, c_loc(nullified_key), value)
    end function

    function multio_metadata_set_string(metadata, key, value) result(err)
        class(multio_metadata), intent(in) :: metadata
        character(*), intent(in) :: key
        character(*), intent(in) :: value
        integer :: err
        character(:), allocatable, target :: nullified_key
        character(:), allocatable, target :: nullified_value

        nullified_key = trim(key) // c_null_char
        nullified_value = trim(value) // c_null_char

        err = c_multio_metadata_set_string(metadata%impl, c_loc(nullified_key), c_loc(nullified_value))
    end function

end module multio_api
