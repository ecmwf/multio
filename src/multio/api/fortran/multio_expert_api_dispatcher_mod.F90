!> @file
!!
!! @brief Definition of an object that can be utilized as the dispatcher runner
!!        for the MultIO server-side execution
!!

module multio_expert_api_dispatcher_mod

    use, intrinsic :: iso_c_binding, only: c_int
    use, intrinsic :: iso_c_binding, only: c_ptr
    use, intrinsic :: iso_c_binding, only: c_null_ptr

implicit none

    ! Default visibility of the module
    private

    !>
    !! @class datatype used to wrap the functionalities of a multio_dispatcher object
    type :: multio_dispatcher

        !! Deafult visibility of the members
        private

        !! Pointer to the opaque c object
        type(c_ptr) :: impl = c_null_ptr

    contains
        procedure, public, pass :: new   => multio_new_dispatcher
        procedure, public, pass :: c_ptr => multio_dispatcher_c_ptr

        procedure, public, pass :: delete => multio_delete_dispatcher
        procedure, public, pass :: run    => multio_dispatcher_run
    end type ! multio_dispatcher

    ! Public symbols whitelist
    public :: multio_dispatcher

contains

    !> @brief Extract the C pointer of the dispatcher object.
    !!
    !! This subroutine extracts and returns the C pointer associated with the
    !! dispatcher object handled by the provided dispatcher context.
    !!
    !! @param[in] d - A handle to the dispatcher object.
    !!
    !! @return A C pointer to the dispatcher object.
    !!         If the object is not available, NULL pointer is returned.
    !!
    function multio_dispatcher_c_ptr( d ) result(loc)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_ptr
        use, intrinsic :: iso_c_binding, only: c_null_ptr
    implicit none
        ! Dummy arguments
        class(multio_dispatcher), intent(in) :: d
        ! Function result
        type(c_ptr), value :: loc
#if !defined(MULTIO_DUMMY_API)
        ! Implementation
        loc = d%impl
#else
        loc = c_null_ptr
#endif
        ! Exit point
        return
    end function multio_dispatcher_c_ptr


    !> @brief Create a new dispatcher object.
    !!
    !! This subroutine creates a new dispatcher object and returns an error code
    !! indicating the operation's success.
    !!
    !! @param[in,out] d  - A handle passed as an object pointer.
    !! @param[in]     cc - The configuration context.
    !! @param[in]     tp - The transport progress context.
    !! @param[in]     q  - The queue context.
    !! @param[in]     ps - The profiler state context.
    !! @param[in]     index - Index of the dispatcher instance to use.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_delete_dispatcher
    !!
    function multio_new_dispatcher(d, cc, tp, q, ps, index) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        ! Variable references from the project
        use :: multio_api_constants_mod,          only: MULTIO_SUCCESS
        use :: multio_api_configuration_mod,      only: multio_configuration
        use :: multio_expert_api_transport_progress_mod, only: multio_transport_progress
        use :: multio_expert_api_queue_mod,             only: multio_queue
        use :: multio_expert_api_profiler_state_mod,    only: multio_profiler_state
    implicit none
        ! Dummy arguments
        class(multio_dispatcher),         intent(out)   :: d
        class(multio_configuration),      intent(inout) :: cc
        class(multio_transport_progress), intent(inout) :: tp
        class(multio_queue),              intent(inout) :: q
        class(multio_profiler_state),     intent(inout) :: ps
        integer(c_int),                   intent(in)    :: index
        ! Function Result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_new_dispatcher(d, cc, tp, q, ps, index) result(err) &
                bind(c, name='multio_new_dispatcher')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), intent(out) :: d
                type(c_ptr), value, intent(in) :: cc
                type(c_ptr), value, intent(in) :: tp
                type(c_ptr), value, intent(in) :: q
                type(c_ptr), value, intent(in) :: ps
                integer(c_int), value, intent(in) :: index
                integer(c_int) :: err
            end function c_multio_new_dispatcher
        end interface
        ! Call the c API
        c_err = c_multio_new_dispatcher(d%impl, cc%c_ptr(), tp%c_ptr(), q%c_ptr(), ps%c_ptr(), index)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_new_dispatcher


    !> @brief Run the dispatcher.
    !!
    !! This subroutine runs the dispatcher associated with the given dispatcher handle.
    !! It returns an error code indicating the operation's success.
    !!
    !! @param[in,out] d - A handle passed as an object pointer.
    !!
    !! @return An error code indicating the operation's success.
    !!
    function multio_dispatcher_run(d) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_dispatcher), intent(inout) :: d
        ! Function Result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_dispatcher_run(d) result(err) &
                bind(c, name='multio_dispatcher_run')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: d
                integer(c_int) :: err
            end function c_multio_dispatcher_run
        end interface
        ! Call the c API
        c_err = c_multio_dispatcher_run(d%impl)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_dispatcher_run


    !> @brief Delete a dispatcher object.
    !!
    !! This subroutine deletes the specified dispatcher object referenced by the provided handle
    !! and returns an error code indicating the operation's success.
    !!
    !! @param[in,out] d - A handle passed as an object pointer.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_new_dispatcher
    !!
    function multio_delete_dispatcher(d) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_null_ptr
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_dispatcher), intent(inout) :: d
        ! Function Result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_delete_dispatcher(d) result(err) &
                bind(c, name='multio_delete_dispatcher')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: d
                integer(c_int) :: err
            end function c_multio_delete_dispatcher
        end interface
        ! Call the c API
        c_err = c_multio_delete_dispatcher(d%impl)
        d%impl = c_null_ptr
        ! Output cast and cleanup
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_delete_dispatcher

end module multio_expert_api_dispatcher_mod