!> @file
!!
!! @brief Definition of an object that can be utilized as the queue
!!        for the MultIO server-side execution
!!

module multio_expert_api_queue_mod

    use, intrinsic :: iso_c_binding, only: c_int
    use, intrinsic :: iso_c_binding, only: c_ptr
    use, intrinsic :: iso_c_binding, only: c_null_ptr

implicit none

    ! Default visibility of the module
    private

    !>
    !! @class datatype used to wrap the functionalities of a multio_queue object
    type :: multio_queue

        !! Deafult visibility of the members
        private

        !! Pointer to the opaque c object
        type(c_ptr) :: impl = c_null_ptr

    contains
        procedure, public, pass :: new   => multio_new_queue
        procedure, public, pass :: c_ptr => multio_queue_c_ptr

        procedure, public, pass :: close  => multio_queue_close
        procedure, public, pass :: delete => multio_delete_queue
    end type ! multio_queue

    ! Public symbols whitelist
    public :: multio_queue

contains

    !> @brief Extract the C pointer of the queue object.
    !!
    !! @param[in,out] q - A handle to the queue object.
    !!
    !! @return A C pointer to the queue object.
    !!
    function multio_queue_c_ptr( q ) result(loc)
        use, intrinsic :: iso_c_binding, only: c_ptr
        use, intrinsic :: iso_c_binding, only: c_null_ptr
    implicit none
        class(multio_queue), target, intent(inout) :: q
        type(c_ptr) :: loc
#if !defined(MULTIO_DUMMY_API)
        loc = q%impl
#else
        loc = c_null_ptr
#endif
        return
    end function multio_queue_c_ptr


    !> @brief Create a new queue object.
    !!
    !! @param[in,out] q        - Queue handle.
    !! @param[in]     ps       - Profiler state context.
    !! @param[in]     capacity - Queue capacity.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_delete_queue
    !!
    function multio_new_queue(q, ps, capacity) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use :: multio_api_constants_mod,        only: MULTIO_SUCCESS
        use :: multio_expert_api_profiler_state_mod, only: multio_profiler_state
    implicit none
        class(multio_queue),          intent(out)   :: q
        class(multio_profiler_state), intent(inout) :: ps
        integer(c_int),               intent(in)    :: capacity
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        integer(kind=c_int) :: c_err
        interface
            function c_multio_new_queue(q, ps, capacity) result(err) &
                bind(c, name='multio_new_queue')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), intent(out) :: q
                type(c_ptr), value, intent(in) :: ps
                integer(c_int), value, intent(in) :: capacity
                integer(c_int) :: err
            end function c_multio_new_queue
        end interface

        c_err = c_multio_new_queue(q%impl, ps%c_ptr(), capacity)
        err   = int(c_err, kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        return
    end function multio_new_queue


    !> @brief Close the queue.
    !!
    !! @param[in,out] q - Queue handle.
    !!
    !! @return An error code indicating the operation's success.
    !!
    function multio_queue_close(q) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        class(multio_queue), intent(inout) :: q
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        integer(kind=c_int) :: c_err
        interface
            function c_multio_queue_close(q) result(err) &
                bind(c, name='multio_queue_close')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: q
                integer(c_int) :: err
            end function c_multio_queue_close
        end interface

        c_err = c_multio_queue_close(q%impl)
        err   = int(c_err, kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        return
    end function multio_queue_close


    !> @brief Delete a queue object.
    !!
    !! @param[in,out] q - Queue handle.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_new_queue
    !!
    function multio_delete_queue(q) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_null_ptr
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        class(multio_queue), intent(inout) :: q
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        integer(kind=c_int) :: c_err
        interface
            function c_multio_delete_queue(q) result(err) &
                bind(c, name='multio_delete_queue')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: q
                integer(c_int) :: err
            end function c_multio_delete_queue
        end interface

        c_err  = c_multio_delete_queue(q%impl)
        q%impl = c_null_ptr
        err    = int(c_err, kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        return
    end function multio_delete_queue

end module multio_expert_api_queue_mod