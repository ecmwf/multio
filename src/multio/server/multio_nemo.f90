
module multio_nemo

    use, intrinsic :: iso_c_binding
    implicit none

    public multio_open_connections, multio_close_connections
    public multio_write_step_complete
    public multio_init_server
    public multio_metadata_set_int_value
    public multio_metadata_set_string_value
    public multio_init_client
    public multio_set_domain
    public multio_write_field
    public multio_field_is_active
    public multio_not_implemented

    integer, private, parameter :: dp = selected_real_kind(15, 307)

    private to_c_string

    interface

        subroutine multio_open_connections() bind(c)
            use, intrinsic :: iso_c_binding
            implicit none
        end subroutine multio_open_connections

        subroutine multio_close_connections() bind(c)
            use, intrinsic :: iso_c_binding
            implicit none
        end subroutine multio_close_connections

        subroutine multio_write_step_complete() bind(c)
            use, intrinsic :: iso_c_binding
            implicit none
        end subroutine multio_write_step_complete

        subroutine multio_init_server(nemo_comm) bind(c)
            use, intrinsic :: iso_c_binding
            implicit none
            integer(c_int), intent(in), value :: nemo_comm
        end subroutine multio_init_server

        subroutine c_multio_metadata_set_int_value(key, val) bind(c, name='multio_metadata_set_int_value')
            use, intrinsic :: iso_c_binding
            implicit none
            character(c_char), intent(in) :: key(*)
            integer(c_int), intent(in), value :: val
        end subroutine c_multio_metadata_set_int_value

        subroutine c_multio_metadata_set_string_value(key, val) bind(c, name='multio_metadata_set_string_value')
            use, intrinsic :: iso_c_binding
            implicit none
            character(c_char), intent(in) :: key(*)
            character(c_char), intent(in) :: val(*)
        end subroutine c_multio_metadata_set_string_value

        function c_multio_init_client(c_name, parent_comm) result(ret_comm) bind(c, name='multio_init_client')
            use, intrinsic :: iso_c_binding
            implicit none
            character(c_char), intent(in) :: c_name(*)
            integer(c_int), intent(in), value :: parent_comm
            integer(c_int) :: ret_comm
        end function c_multio_init_client

        subroutine c_multio_set_domain(c_key, data, size) bind(c, name='multio_set_domain')
            use, intrinsic :: iso_c_binding
            implicit none
            character(c_char), intent(in) :: c_key(*)
            integer(c_int), dimension(*), intent(in) :: data
            integer(c_int), intent(in), value :: size
        end subroutine c_multio_set_domain

        subroutine c_multio_write_field(c_name, data, sz, toall) bind(c, name='multio_write_field')
            use, intrinsic :: iso_c_binding
            implicit none
            character(c_char), intent(in) :: c_name(*)
            real(c_double), dimension(*), intent(in) :: data
            integer(c_int), intent(in), value :: sz
            logical(c_bool), intent(in) :: toall
        end subroutine c_multio_write_field

        function c_multio_field_is_active(c_name) result(is_active) bind(c, name='multio_field_is_active')
            use, intrinsic :: iso_c_binding
            implicit none
            character(c_char), intent(in) :: c_name(*)
            logical(c_bool) :: is_active
        end function c_multio_field_is_active

        subroutine c_multio_not_implemented(c_message) bind(c, name='multio_not_implemented')
            use, intrinsic :: iso_c_binding
            implicit none
            character(c_char), intent(in) :: c_message(*)
        end subroutine c_multio_not_implemented

    end interface

    contains

        function to_c_string(fstr) result(cstr)
            implicit none
            character(*), intent(in) :: fstr
            character(:), allocatable :: cstr

            cstr = trim(fstr) // c_null_char

        end function to_c_string

        subroutine multio_metadata_set_int_value(key, val)
            implicit none
            character(*), intent(in) :: key
            integer, intent(in)      :: val

            call c_multio_metadata_set_int_value(to_c_string(key), val)

        end subroutine multio_metadata_set_int_value

        subroutine multio_metadata_set_string_value(key, val)
            implicit none
            character(*), intent(in) :: key
            character(*), intent(in) :: val

            call c_multio_metadata_set_string_value(to_c_string(key), to_c_string(val))

        end subroutine multio_metadata_set_string_value

        subroutine multio_init_client(name, ret_comm, parent_comm)
            implicit none
            character(*), intent(in) :: name
            integer, intent(in)  :: parent_comm
            integer, intent(out) :: ret_comm

            ret_comm = c_multio_init_client(to_c_string(name), parent_comm)

        end subroutine multio_init_client

        subroutine multio_set_domain(key, data)
            implicit none
            character(*), intent(in) :: key
            integer, dimension(:), intent(in) :: data

            call c_multio_set_domain(to_c_string(key), data, size(data))

        end subroutine multio_set_domain

        subroutine multio_write_field(fname, data, toall)
            implicit none
            character(*), intent(in) :: fname
            real(dp), dimension(:,:), intent(in) :: data
            logical(c_bool), optional, intent(in) :: toall
            logical(c_bool) :: c_toall = .false.

            if (present(toall)) c_toall = toall

            call c_multio_write_field(to_c_string(fname), data, size(data), toall)

        end subroutine multio_write_field

        subroutine multio_field_is_active(fname, is_active)
            implicit none
            character(*), intent(in) :: fname
            logical, intent(out) :: is_active

            is_active = c_multio_field_is_active(to_c_string(fname))

        end subroutine multio_field_is_active

        subroutine multio_not_implemented(message)
            implicit none
            character(*), intent(in) :: message

            call c_multio_not_implemented(to_c_string(message))

        end subroutine multio_not_implemented


end module multio_nemo
