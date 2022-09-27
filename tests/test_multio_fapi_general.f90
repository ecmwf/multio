module test_multio_fapi_general
    use multio_config
    use multio_api
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    implicit none

    integer :: test_error_handler_calls = 0
    integer(8) :: test_error_handler_last_context
    integer :: test_error_handler_last_error

contains

    function test_multio_version() result(success)

        ! Test that we obtain the expected version number

        logical :: success
        character(:), allocatable :: version_str

        success = .true.

        if (multio_version(version_str) /= MULTIO_SUCCESS) then
            write(error_unit, *) 'getting version string failed'
            success = .false.
        end if

        if (version_str /= multio_version_str) then
            write(error_unit, *) "Unexpected version: ", version_str
            write(error_unit, *) "Expected: ", multio_version_str
            success = .false.
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
        end if

        if (sha1 /= multio_git_sha1_str .and. sha1 /= "not available") then
            write(error_unit, *) "Unexpected git sha1: ", sha1
            write(error_unit, *) "Expected: ", multio_git_sha1_str
            success = .false.
        endif

    end function

    function test_error_handling() result(success)
        logical :: success
        type(multio_configurationcontext) :: cc
        integer :: j, err
        success = .true.
        
        do j = 1, 2
            err = cc%new_from_filename('invalid-path')
            if (err == MULTIO_SUCCESS) then
                write(error_unit, *) 'multio_configurationcontext%new_from_filename succeeded unexpectedly with "invalid-path"'
                success = .false.
            end if
            
            if (multio_error_string(err) /= "Cannot open invalid-path  (No such file or directory)") then
                write(error_unit, *) 'unexpected error message: ', multio_error_string(err)
                success = .false.
            endif
            
            ! err = cc%delete()
            ! if (err /= MULTIO_SUCCESS) then
            !     write(error_unit, *) 'Could not delete configuration context'
            !     success = .false.
            ! end if
            
            ! if (.not. c_associated(cc%impl)) then
            !     write(error_unit, *) 'configurationcontext ptr has not been reset'
            !     success = .false.
            ! end if
        end do
    end function

    function test_multio_set_failure_handler() result(success)

        ! Test that we can set failure handler and that it is being called on error with appropriate information

        logical :: success
        integer(8) :: original_context = 123456
        integer(8) :: context 
        integer :: err
        type(multio_configurationcontext) :: cc
        type(multio_handle) :: mio
        character(:), allocatable :: name

        success = .true.
        context = original_context

        ! Set test error handler and its context
        if (multio_set_failure_handler(test_error_handler, context) /= MULTIO_SUCCESS) then
            write(error_unit, *) 'setting failure handler failed'
            success = .false.
        end if

        ! Trigger an error
        err = cc%new_from_filename("invalid-path")
        if (err == MULTIO_SUCCESS) then
            write(error_unit, *) 'multio_configurationcontext%new_from_filename succeeded unexpectedly with "invalid-path"'
            success = .false.
        end if

        ! Check number of error handler calls
        if (test_error_handler_calls /= 1) then
            write(error_unit, *) 'error handler not called expected number of times:', test_error_handler_calls, '/=', 1
            success = .false.
        end if

        ! Check last received error handler context
        if (test_error_handler_last_context /= context) then
            write(error_unit, *) 'error handler context differs:', test_error_handler_last_context, '/=', context
            success = .false.
        end if

        ! Check last received error code
        if (test_error_handler_last_error /= err) then
            write(error_unit, *) 'error handler error code differs:', test_error_handler_last_error, '/=', err
            success = .false.
        end if

        ! Change context value
        context = 654321
        
        err = cc%new()
        if (err /= MULTIO_SUCCESS) then
            write(error_unit, *) 'multio_configurationcontext%new failed unexpectedly: ',multio_error_string(err)
            success = .false.
        end if
        
        err = cc%mpi_allow_world_default_comm( .FALSE._1 )
        if (err /= MULTIO_SUCCESS) then
            write(error_unit, *) 'multio_configurationcontext%allowWorldAsDefault failed unexpectedly: ',multio_error_string(err)
            success = .false.
        end if

        ! Trigger another error
        err = mio%new(cc)
        if (err == MULTIO_SUCCESS) then
            write(error_unit, *) 'multio_new succeeded unexpectedly with "0"'
            success = .false.
        end if

        ! Check number of error handler calls
        if (test_error_handler_calls /= 2) then
            write(error_unit, *) 'error handler not called expected number of times:', test_error_handler_calls, '/=', 2
            success = .false.
        end if

        ! Check last received error handler context, it should match the original one
        if (test_error_handler_last_context /= original_context) then
            write(error_unit, *) 'error handler context differs:', test_error_handler_last_context, '/=', original_context
            success = .false.
        end if

        ! Check last received error code, it should match the latest one
        if (test_error_handler_last_error /= err) then
            write(error_unit, *) 'error handler error code differs:', test_error_handler_last_error, '/=', err
            success = .false.
        end if

        ! Change context value another time
        context = 0

        ! Trigger yet another error
        err = multio_start_server(cc, "test-server")
        if (err == MULTIO_SUCCESS) then
            write(error_unit, *) 'multio_start_server succeeded unexpectedly with invalid configuration context'
            success = .false.
        end if

        ! Check number of error handler calls
        if (test_error_handler_calls /= 3) then
            write(error_unit, *) 'error handler not called expected number of times:', test_error_handler_calls, '/=', 3
            success = .false.
        end if

        ! Check last received error handler context, it should match the original one
        if (test_error_handler_last_context /= original_context) then
            write(error_unit, *) 'error handler context differs:', test_error_handler_last_context, '/=', original_context
            success = .false.
        end if

        ! Check last received error code, it should match the latest one
        if (test_error_handler_last_error /= err) then
            write(error_unit, *) 'error handler error code differs:', test_error_handler_last_error, '/=', err
            success = .false.
        end if
    end function

    subroutine test_error_handler(context, error)
        integer(8), intent(inout) :: context
        integer, intent(in) :: error

        test_error_handler_calls = test_error_handler_calls + 1
        test_error_handler_last_context = context
        test_error_handler_last_error = error
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
