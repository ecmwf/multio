module test_multio_fapi_general
    use multio_config
    use multio_api
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    implicit none

    integer :: test_error_handler_calls = 0
    integer(8) :: test_error_handler_last_context
    integer :: test_error_handler_last_error

    integer :: test_error_handler_calls2 = 0
    integer(8) :: test_error_handler_last_context2
    integer :: test_error_handler_last_error2

    integer :: test_error_handler_calls3 = 0
    integer(8) :: test_error_handler_last_context3
    integer :: test_error_handler_last_error3

contains

    function test_multio_version() result(success)

        ! Test that we obtain the expected version number

        logical :: success
        character(:), allocatable :: version_str

        success = .true.

        if (multio_version(version_str) /= MULTIO_SUCCESS) then
            write(error_unit, *) 'getting version string failed'
            success = .false.
            return
        end if

        if (version_str /= multio_version_str) then
            write(error_unit, *) "Unexpected version: ", version_str
            write(error_unit, *) "Expected: ", multio_version_str
            success = .false.
            return
        endif

    end function

    function test_git_sha1() result(success)

        ! Test that we obtain the expected version number

        logical :: success
        character(:), allocatable :: sha1

        success = .true.

        if (multio_vcs_version(sha1) /= MULTIO_SUCCESS) then
            write(error_unit, *) 'getting git sha1 string failed'
            success = .false.
            return
        end if

        if (sha1 /= multio_git_sha1_str .and. sha1 /= "not available") then
            write(error_unit, *) "Unexpected git sha1: ", sha1
            write(error_unit, *) "Expected: ", multio_git_sha1_str
            success = .false.
            return
        endif

    end function

    function test_error_handling() result(success)
        logical :: success
        type(multio_configuration) :: cc
        integer :: j, err
        success = .true.

        do j = 1, 2
            err = cc%new('invalid-path')
            if (err == MULTIO_SUCCESS) then
                write(error_unit, *) 'multio_configuration%new (new_from_filename) succeeded unexpectedly with "invalid-path"'
                success = .false.
                return
            end if

            if (TRIM(multio_error_string(err)) /= "Caught eckit exception on C-C++ API boundary: &
            &Cannot open invalid-path  (No such file or directory)") then
                write(error_unit, *) 'unexpected error message: ', multio_error_string(err)
                success = .false.
                return
            endif

            ! err = cc%delete()
            ! if (err /= MULTIO_SUCCESS) then
            !     write(error_unit, *) 'Could not delete configuration'
            !     success = .false.
            !     return
            ! end if

            ! if (.not. c_associated(cc%impl)) then
            !     write(error_unit, *) 'configuration ptr has not been reset'
            !     success = .false.
            !     return
            ! end if
        end do
    end function

    function test_multio_set_failure_handler() result(success)

        ! Test that we can set failure handler and that it is being called on error with appropriate information

        logical :: success
        integer(8) :: original_context = 123456
        integer(8) :: context
        integer(8) :: context2
        integer(8) :: context3
        integer :: err
        type(multio_configuration) :: cc
        type(multio_handle) :: mio
        type(multio_configuration) :: cc2
        type(multio_handle) :: mio2
        type(multio_configuration) :: cc3
        type(multio_handle) :: mio3
        character(:), allocatable :: name

        success = .true.
        context = original_context

        ! Trigger an error
        ! err = cc%new("invalid-path")
        ! if (err == MULTIO_SUCCESS) then
        !     write(error_unit, *) 'multio_configuration%new (new_from_filename) succeeded unexpectedly with "invalid-path"'
        !     success = .false.
        !     return
        ! end if
        err = cc%new()
        if (err /= MULTIO_SUCCESS) then
            write(error_unit, *) 'multio_configuration%new failed'
            success = .false.
            return
        end if

        ! Set test error handler and its context
        if (cc%set_failure_handler(test_error_handler, context) /= MULTIO_SUCCESS) then
            write(error_unit, *) 'setting failure handler failed: ',multio_error_string(err)
            success = .false.
            return
        end if

        ! Check number of error handler calls
        if (test_error_handler_calls /= 0) then
            write(error_unit, *) 'error handler not called expected number of times:', test_error_handler_calls, '/=', 0
            success = .false.
            return
        end if

        ! Check last received error handler context
        if (test_error_handler_last_context /= 0) then
            write(error_unit, *) 'error handler context differs:', test_error_handler_last_context, '/=', context
            success = .false.
            return
        end if

        ! Check last received error code
        if (test_error_handler_last_error /= err) then
            write(error_unit, *) 'error handler error code differs:', test_error_handler_last_error, '/=', err
            success = .false.
            return
        end if

        ! ! Change context value
        ! context = 654321

        ! err = cc%new()
        ! if (err /= MULTIO_SUCCESS) then
        !     write(error_unit, *) 'multio_configuration%new failed unexpectedly: ',multio_error_string(err)
        !     success = .false.
        !     return
        ! end if

        err = cc%mpi_allow_world_default_comm( .FALSE._1 )
        if (err /= MULTIO_SUCCESS) then
            write(error_unit, *) 'multio_configuration%allowWorldAsDefault failed unexpectedly: ',multio_error_string(err)
            success = .false.
            return
        end if

        ! Trigger another error
        err = mio%new(cc)
        if (err == MULTIO_SUCCESS) then
            write(error_unit, *) 'multio_new succeeded unexpectedly with "0"'
            success = .false.
            return
        end if

        ! Check number of error handler calls
        if (test_error_handler_calls /= 1) then
            write(error_unit, *) 'error handler not called expected number of times:', test_error_handler_calls, '/=', 1
            success = .false.
            return
        end if

        ! Check last received error handler context, it should match the original one
        if (test_error_handler_last_context /= original_context) then
            write(error_unit, *) 'error handler context differs:', test_error_handler_last_context, '/=', original_context
            success = .false.
            return
        end if

        ! Check last received error code, it should match the latest one
        if (test_error_handler_last_error /= err) then
            write(error_unit, *) 'error handler error code differs:', test_error_handler_last_error, '/=', err
            success = .false.
            return
        end if


        ! Create other handles and test behaviour
        context2 = 22
        context3 = 33
        err = cc2%new()
        if (err /= MULTIO_SUCCESS) then
            write(error_unit, *) 'multio_configuration%new failed (2)'
            success = .false.
            return
        end if
        if (cc2%set_failure_handler(test_error_handler2, context2) /= MULTIO_SUCCESS) then
            write(error_unit, *) 'setting failure handler failed (2): ',multio_error_string(err)
            success = .false.
            return
        end if
        err = cc3%new()
        if (err /= MULTIO_SUCCESS) then
            write(error_unit, *) 'multio_configuration%new failed (3)'
            success = .false.
            return
        end if
        if (cc3%set_failure_handler(test_error_handler3, context3) /= MULTIO_SUCCESS) then
            write(error_unit, *) 'setting failure handler failed (3): ',multio_error_string(err)
            success = .false.
            return
        end if

        ! Now remove second handler and readd
        err = cc2%delete()
        if (err /= MULTIO_SUCCESS) then
            write(error_unit, *) 'Could not delete configuration (2)'
            success = .false.
            return
        end if
        err = cc2%new()
        if (err /= MULTIO_SUCCESS) then
            write(error_unit, *) 'multio_configuration%new failed (2,2)'
            success = .false.
            return
        end if
        if (cc2%set_failure_handler(test_error_handler2, context2) /= MULTIO_SUCCESS) then
            write(error_unit, *) 'setting failure handler failed (2,2): ',multio_error_string(err)
            success = .false.
            return
        end if

        ! Trigger error on 2
        err = cc2%mpi_allow_world_default_comm( .FALSE._1 )
        err = mio2%new(cc2)
        if (err == MULTIO_SUCCESS) then
            write(error_unit, *) 'multio_new (2) succeeded unexpectedly with "0"'
            success = .false.
            return
        end if


        ! Check number of error handler calls
        if (test_error_handler_calls2 /= 1) then
            write(error_unit, *) 'error handler (2) not called expected number of times:', test_error_handler_calls2, '/=', 1
            success = .false.
            return
        end if

        ! Check last received error handler context, it should match the original one
        if (test_error_handler_last_context2 /= 22) then
            write(error_unit, *) 'error handler context differs:', test_error_handler_last_context2, '/=', 22
            success = .false.
            return
        end if

        ! Check last received error code, it should match the latest one
        if (test_error_handler_last_error2 /= err) then
            write(error_unit, *) 'error handler error code differs:', test_error_handler_last_error2, '/=', err
            success = .false.
            return
        end if


        ! Trigger error on 3
        err = cc3%mpi_allow_world_default_comm( .FALSE._1 )
        err = mio3%new(cc3)
        if (err == MULTIO_SUCCESS) then
            write(error_unit, *) 'multio_new (3) succeeded unexpectedly with "0"'
            success = .false.
            return
        end if

        ! Check number of error handler calls
        if (test_error_handler_calls3 /= 1) then
            write(error_unit, *) 'error handler (3) not called expected number of times:', test_error_handler_calls3, '/=', 1
            success = .false.
            return
        end if

        ! Check last received error handler context, it should match the original one
        if (test_error_handler_last_context3 /= 33) then
            write(error_unit, *) 'error handler context differs:', test_error_handler_last_context3, '/=', 33
            success = .false.
            return
        end if

        ! Check last received error code, it should match the latest one
        if (test_error_handler_last_error3 /= err) then
            write(error_unit, *) 'error handler error code differs:', test_error_handler_last_error3, '/=', err
            success = .false.
            return
        end if



        ! Back to first handle
        ! Change context value another time
        context = 0

        ! Trigger yet another error
        err = multio_start_server(cc)
        if (err == MULTIO_SUCCESS) then
            write(error_unit, *) 'multio_start_server succeeded unexpectedly with invalid configuration context'
            success = .false.
            return
        end if

        ! Check number of error handler calls
        if (test_error_handler_calls /= 2) then
            write(error_unit, *) 'error handler not called expected number of times:', test_error_handler_calls, '/=', 2
            success = .false.
            return
        end if

        ! Check last received error handler context, it should match the original one
        if (test_error_handler_last_context /= original_context) then
            write(error_unit, *) 'error handler context differs:', test_error_handler_last_context, '/=', original_context
            success = .false.
            return
        end if

        ! Check last received error code, it should match the latest one
        if (test_error_handler_last_error /= err) then
            write(error_unit, *) 'error handler error code differs:', test_error_handler_last_error, '/=', err
            success = .false.
            return
        end if
    end function

    subroutine test_error_handler(context, error, info)
        integer(8), intent(inout) :: context
        integer, intent(in) :: error
        class(multio_failure_info), intent(in) :: info

        test_error_handler_calls = test_error_handler_calls + 1
        test_error_handler_last_context = context
        test_error_handler_last_error = error
    end subroutine

    subroutine test_error_handler2(context, error, info)
        integer(8), intent(inout) :: context
        integer, intent(in) :: error
        class(multio_failure_info), intent(in) :: info

        test_error_handler_calls2 = test_error_handler_calls2 + 1
        test_error_handler_last_context2 = context
        test_error_handler_last_error2 = error
    end subroutine

    subroutine test_error_handler3(context, error, info)
        integer(8), intent(inout) :: context
        integer, intent(in) :: error
        class(multio_failure_info), intent(in) :: info

        test_error_handler_calls3 = test_error_handler_calls3 + 1
        test_error_handler_last_context3 = context
        test_error_handler_last_error3 = error
    end subroutine

end module


program fapi_general

    use test_multio_fapi_general
    implicit none

    logical :: success
    success = .true.

    if (multio_initialise() /= MULTIO_SUCCESS) then
        write(error_unit, *) 'Failed to initialise MULTIO api'
        success = .false.
    end if

    success = success .and. test_multio_version()
    success = success .and. test_git_sha1()
    success = success .and. test_error_handling()
    success = success .and. test_multio_set_failure_handler()

    if (.not. success) stop -1

end program
