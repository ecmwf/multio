!> @file
!!
!! @brief Definition of an object that can be utilized as the profiler state
!!        for the MultIO server-side execution
!!

module multio_expert_api_profiler_state_mod

    use, intrinsic :: iso_c_binding, only: c_int
    use, intrinsic :: iso_c_binding, only: c_ptr
    use, intrinsic :: iso_c_binding, only: c_null_ptr

implicit none

    ! Default visibility of the module
    private

    !>
    !! @class datatype used to wrap the functionalities of a multio_profiler_state object
    type :: multio_profiler_state

        !! Deafult visibility of the members
        private

        !! Pointer to the opaque c object
        type(c_ptr) :: impl = c_null_ptr

    contains
        procedure, public, pass :: new   => multio_new_profiler_state
        procedure, public, pass :: c_ptr => multio_profiler_state_c_ptr

        procedure, public, pass :: delete => multio_delete_profiler_state
    end type ! multio_profiler_state

    ! Public symbols whitelist
    public :: multio_profiler_state

contains

    !> @brief Extract the C pointer of the profiler state object.
    !!
    !! @param[in,out] ps - A handle to the profiler state object.
    !!
    !! @return A C pointer to the profiler state object.
    !!
    function multio_profiler_state_c_ptr( ps ) result(loc)
        use, intrinsic :: iso_c_binding, only: c_ptr
        use, intrinsic :: iso_c_binding, only: c_null_ptr
    implicit none
        class(multio_profiler_state), target, intent(inout) :: ps
        type(c_ptr) :: loc
#if !defined(MULTIO_DUMMY_API)
        loc = ps%impl
#else
        loc = c_null_ptr
#endif
        return
    end function multio_profiler_state_c_ptr


    !> @brief Create a new profiler state object.
    !!
    !! @param[in,out] ps             - Profiler state handle.
    !! @param[in]     numDispatchers - Number of dispatcher instances.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_delete_profiler_state
    !!
    function multio_new_profiler_state(ps, numDispatchers) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        class(multio_profiler_state), intent(out) :: ps
        integer(c_int),               intent(in)  :: numDispatchers
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        integer(kind=c_int) :: c_err
        interface
            function c_multio_new_profiler_state(ps, numDispatchers) result(err) &
                bind(c, name='multio_new_profiler_state')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), intent(out) :: ps
                integer(c_int), value, intent(in) :: numDispatchers
                integer(c_int) :: err
            end function c_multio_new_profiler_state
        end interface

        c_err = c_multio_new_profiler_state(ps%impl, numDispatchers)
        err   = int(c_err, kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        return
    end function multio_new_profiler_state


    !> @brief Delete a profiler state object.
    !!
    !! @param[in,out] ps - Profiler state handle.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_new_profiler_state
    !!
    function multio_delete_profiler_state(ps) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_null_ptr
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        class(multio_profiler_state), intent(inout) :: ps
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        integer(kind=c_int) :: c_err
        interface
            function c_multio_delete_profiler_state(ps) result(err) &
                bind(c, name='multio_delete_profiler_state')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: ps
                integer(c_int) :: err
            end function c_multio_delete_profiler_state
        end interface

        c_err   = c_multio_delete_profiler_state(ps%impl)
        ps%impl = c_null_ptr
        err     = int(c_err, kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        return
    end function multio_delete_profiler_state

end module multio_expert_api_profiler_state_mod