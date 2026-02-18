!> @file
!!
!! @brief Definition of an object that can be utilized as the transport progress
!!        for the MultIO server-side execution
!!

module multio_expert_api_transport_progress_mod

    use, intrinsic :: iso_c_binding, only: c_int
    use, intrinsic :: iso_c_binding, only: c_ptr
    use, intrinsic :: iso_c_binding, only: c_null_ptr

implicit none

    ! Default visibility of the module
    private

    !>
    !! @class datatype used to wrap the functionalities of a multio_transport_progress object
    type :: multio_transport_progress

        !! Deafult visibility of the members
        private

        !! Pointer to the opaque c object
        type(c_ptr) :: impl = c_null_ptr

    contains
        procedure, public, pass :: new   => multio_new_transport_progress
        procedure, public, pass :: c_ptr => multio_transport_progress_c_ptr

        procedure, public, pass :: run    => multio_transport_progress_run
        procedure, public, pass :: stop   => multio_transport_progress_stop
        procedure, public, pass :: delete => multio_delete_transport_progress
    end type ! multio_transport_progress

    ! Public symbols whitelist
    public :: multio_transport_progress

contains

    !> @brief Extract the C pointer of the transport progress object.
    !!
    !! @param[in,out] tp - A handle to the transport progress object.
    !!
    !! @return A C pointer to the transport progress object.
    !!
    function multio_transport_progress_c_ptr( tp ) result(loc)
        use, intrinsic :: iso_c_binding, only: c_ptr
        use, intrinsic :: iso_c_binding, only: c_null_ptr
    implicit none
        class(multio_transport_progress), target, intent(inout) :: tp
        type(c_ptr) :: loc
#if !defined(MULTIO_DUMMY_API)
        loc = tp%impl
#else
        loc = c_null_ptr
#endif
        return
    end function multio_transport_progress_c_ptr


    !> @brief Create a new transport progress object.
    !!
    !! @param[in,out] tp - Transport progress handle.
    !! @param[in]     cc - Configuration context.
    !! @param[in]     ps - Profiler state context.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_delete_transport_progress
    !!
    function multio_new_transport_progress(tp, cc, ps) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
        use :: multio_api_configuration_mod,           only: multio_configuration
        use :: multio_expert_api_profiler_state_mod,   only: multio_profiler_state
    implicit none
        class(multio_transport_progress), intent(out)   :: tp
        class(multio_configuration),      intent(inout) :: cc
        class(multio_profiler_state),     intent(inout) :: ps
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        integer(kind=c_int) :: c_err
        interface
            function c_multio_new_transport_progress(tp, cc, ps) result(err) &
                bind(c, name='multio_new_transport_progress')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), intent(out) :: tp
                type(c_ptr), value, intent(in) :: cc
                type(c_ptr), value, intent(in) :: ps
                integer(c_int) :: err
            end function c_multio_new_transport_progress
        end interface

        c_err = c_multio_new_transport_progress(tp%impl, cc%c_ptr(), ps%c_ptr())
        err   = int(c_err, kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        return
    end function multio_new_transport_progress


    !> @brief Run the transport progress.
    !!
    !! @param[in,out] tp - Transport progress handle.
    !!
    !! @return An error code indicating the operation's success.
    !!
    function multio_transport_progress_run(tp) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        class(multio_transport_progress), intent(inout) :: tp
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        integer(kind=c_int) :: c_err
        interface
            function c_multio_transport_progress_run(tp) result(err) &
                bind(c, name='multio_transport_progress_run')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: tp
                integer(c_int) :: err
            end function c_multio_transport_progress_run
        end interface

        c_err = c_multio_transport_progress_run(tp%impl)
        err   = int(c_err, kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        return
    end function multio_transport_progress_run


    !> @brief Stop the transport progress.
    !!
    !! @param[in,out] tp - Transport progress handle.
    !!
    !! @return An error code indicating the operation's success.
    !!
    function multio_transport_progress_stop(tp) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        class(multio_transport_progress), intent(inout) :: tp
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        integer(kind=c_int) :: c_err
        interface
            function c_multio_transport_progress_stop(tp) result(err) &
                bind(c, name='multio_transport_progress_stop')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: tp
                integer(c_int) :: err
            end function c_multio_transport_progress_stop
        end interface

        c_err = c_multio_transport_progress_stop(tp%impl)
        err   = int(c_err, kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        return
    end function multio_transport_progress_stop


    !> @brief Delete a transport progress object.
    !!
    !! @param[in,out] tp - Transport progress handle.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_new_transport_progress
    !!
    function multio_delete_transport_progress(tp) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_null_ptr
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        class(multio_transport_progress), intent(inout) :: tp
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        integer(kind=c_int) :: c_err
        interface
            function c_multio_delete_transport_progress(tp) result(err) &
                bind(c, name='multio_delete_transport_progress')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: tp
                integer(c_int) :: err
            end function c_multio_delete_transport_progress
        end interface

        c_err   = c_multio_delete_transport_progress(tp%impl)
        tp%impl = c_null_ptr
        err     = int(c_err, kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        return
    end function multio_delete_transport_progress

end module multio_expert_api_transport_progress_mod