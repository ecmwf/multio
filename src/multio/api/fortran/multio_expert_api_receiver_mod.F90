!> @file
!!
!! @brief Definition of an object that can be utilized as the receiver
!!        for the MultIO server-side execution
!!

module multio_expert_api_receiver_mod

    use, intrinsic :: iso_c_binding, only: c_int
    use, intrinsic :: iso_c_binding, only: c_ptr
    use, intrinsic :: iso_c_binding, only: c_null_ptr

implicit none

    ! Default visibility of the module
    private

    !>
    !! @class datatype used to wrap the functionalities of a multio_receiver object
    type :: multio_receiver

        !! Deafult visibility of the members
        private

        !! Pointer to the opaque c object
        type(c_ptr) :: impl = c_null_ptr

    contains
        procedure, public, pass :: new   => multio_new_receiver
        procedure, public, pass :: c_ptr => multio_receiver_c_ptr

        procedure, public, pass :: run    => multio_receiver_run
        procedure, public, pass :: delete => multio_delete_receiver
    end type ! multio_receiver

    ! Public symbols whitelist
    public :: multio_receiver

contains

    !> @brief Extract the C pointer of the receiver object.
    !!
    !! @param[in,out] r - A handle to the receiver object.
    !!
    !! @return A C pointer to the receiver object.
    !!
    function multio_receiver_c_ptr( r ) result(loc)
        use, intrinsic :: iso_c_binding, only: c_ptr
        use, intrinsic :: iso_c_binding, only: c_null_ptr
    implicit none
        class(multio_receiver), target, intent(inout) :: r
        type(c_ptr) :: loc
#if !defined(MULTIO_DUMMY_API)
        loc = r%impl
#else
        loc = c_null_ptr
#endif
        return
    end function multio_receiver_c_ptr


    !> @brief Create a new receiver object.
    !!
    !! @param[in,out] r  - Receiver handle.
    !! @param[in]     cc - Configuration context.
    !! @param[in]     tp - Transport progress context.
    !! @param[in]     q  - Queue context.
    !! @param[in]     ps - Profiler state context.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_delete_receiver
    !!
    function multio_new_receiver(r, cc, tp, q, ps) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
        use :: multio_api_configuration_mod,           only: multio_configuration
        use :: multio_expert_api_transport_progress_mod, only: multio_transport_progress
        use :: multio_expert_api_queue_mod,              only: multio_queue
        use :: multio_expert_api_profiler_state_mod,     only: multio_profiler_state
    implicit none
        class(multio_receiver),          intent(out)   :: r
        class(multio_configuration),     intent(inout) :: cc
        class(multio_transport_progress),intent(inout) :: tp
        class(multio_queue),             intent(inout) :: q
        class(multio_profiler_state),    intent(inout) :: ps
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        integer(kind=c_int) :: c_err
        interface
            function c_multio_new_receiver(r, cc, tp, q, ps) result(err) &
                bind(c, name='multio_new_receiver')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), intent(out) :: r
                type(c_ptr), value, intent(in) :: cc
                type(c_ptr), value, intent(in) :: tp
                type(c_ptr), value, intent(in) :: q
                type(c_ptr), value, intent(in) :: ps
                integer(c_int) :: err
            end function c_multio_new_receiver
        end interface

        c_err = c_multio_new_receiver(r%impl, cc%c_ptr(), tp%c_ptr(), q%c_ptr(), ps%c_ptr())
        err   = int(c_err, kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        return
    end function multio_new_receiver


    !> @brief Run the receiver.
    !!
    !! @param[in,out] r - Receiver handle.
    !!
    !! @return An error code indicating the operation's success.
    !!
    function multio_receiver_run(r) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        class(multio_receiver), intent(inout) :: r
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        integer(kind=c_int) :: c_err
        interface
            function c_multio_receiver_run(r) result(err) &
                bind(c, name='multio_receiver_run')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: r
                integer(c_int) :: err
            end function c_multio_receiver_run
        end interface

        c_err = c_multio_receiver_run(r%impl)
        err   = int(c_err, kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        return
    end function multio_receiver_run


    !> @brief Delete a receiver object.
    !!
    !! @param[in,out] r - Receiver handle.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_new_receiver
    !!
    function multio_delete_receiver(r) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_null_ptr
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        class(multio_receiver), intent(inout) :: r
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        integer(kind=c_int) :: c_err
        interface
            function c_multio_delete_receiver(r) result(err) &
                bind(c, name='multio_delete_receiver')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: r
                integer(c_int) :: err
            end function c_multio_delete_receiver
        end interface

        c_err   = c_multio_delete_receiver(r%impl)
        r%impl  = c_null_ptr
        err     = int(c_err, kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        return
    end function multio_delete_receiver

end module multio_expert_api_receiver_mod