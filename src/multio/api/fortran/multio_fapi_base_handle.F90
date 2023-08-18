!> @file
!!
!! @brief Definition of the fundamental functionalities for a multio_handle
!!
!! @note This module is kept distinct from the "full" multio handle to prevent circular dependencies.
!!

module multio_api_base_handle_mod

    use, intrinsic :: iso_c_binding, only: c_ptr
    use, intrinsic :: iso_c_binding, only: c_int
    use, intrinsic :: iso_c_binding, only: c_null_ptr

implicit none

    ! Default symbols visibility
    private

    !>
    !! @class datatype used to wrap the functionalities of a multio_base_handle object
    type :: multio_base_handle

        !! Default public visibility of members.
        !! The derived class (multio_handle) needs to access members

        !! Pointer to the opaque c object
        type(c_ptr) :: impl =  c_null_ptr

        !! Pointer to the failure handler
        integer(c_int), pointer :: failure_id => null()

    contains

        ! General management
        procedure, private, pass :: new_handle           => multio_base_handle_new
        procedure, private, pass :: new_handle_default   => multio_base_handle_new_default
        procedure, public,  pass :: delete               => multio_base_handle_delete
        procedure, public,  pass :: c_ptr                => multio_base_handle_c_ptr
        procedure, public,  pass :: set_failure_handler  => multio_base_handle_set_failure_handler
        generic,   public        :: new => new_handle,        &
                                        &  new_handle_default

        ! Connection handling
        procedure, public,  pass :: open_connections     => multio_base_handle_open_connections
        procedure, public,  pass :: close_connections    => multio_base_handle_close_connections

    end type ! multio_base_handle

    ! Public symbols whitelist
    public :: multio_base_handle

contains


    !> @brief Retrieve the C pointer of the handle object
    !!
    !! This function extracts the C pointer associated with the handle object.
    !!
    !! @param [in,out] handle - Pointer to the handle object
    !!
    !! @return Pointer to the C handle object
    !!
    function multio_base_handle_c_ptr( handle ) result(loc)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_ptr
        use, intrinsic :: iso_c_binding, only: c_null_ptr
    implicit none
        ! Dummy arguments
        class(multio_base_handle), target, intent(inout) :: handle
        ! Function result
        type(c_ptr) :: loc
#if !defined(MULTIO_DUMMY_API)
        ! Implementation
        loc = handle%impl
#else
        loc = c_null_ptr
#endif
        ! Exit point
        return
    end function multio_base_handle_c_ptr


    !> @brief Create a new multio handle from a multio configuration
    !!
    !! This function initializes a new multio handle based on the provided
    !! multio configuration object.
    !!
    !! @param [in,out] handle - Pointer to the handle object to be created
    !! @param [in,out] cc     - Pointer to the multio_configuration
    !!                          object used to configure the handle
    !!
    !! @return Error code indicating the operation's success
    !!
    !! @see multio_configuration
    !! @see multio_base_handle_new_default
    !! @see multio_base_handle_delete
    !!
    function multio_base_handle_new(handle, cc) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding,     only: c_int
        ! Variable references from the project
        use :: multio_api_configuration_mod, only: multio_configuration
        use :: multio_api_constants_mod,     only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_base_handle),   intent(inout) :: handle
        class(multio_configuration), intent(inout) :: cc
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_new_handle(handle, cc) result(err) &
                    bind(c, name='multio_new_handle')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),        intent(out) :: handle
                type(c_ptr), value, intent(in)  :: cc
                integer(c_int) :: err
            end function c_multio_new_handle
        end interface
        ! Implementation
        handle%failure_id => cc%move_failure_id()
        c_err = c_multio_new_handle(handle%impl, cc%c_ptr())
        ! Setting return value
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_base_handle_new


    !> @brief Create a default multio handle
    !!
    !! This function initializes a new multio handle with default settings.
    !!
    !! @param [in,out] handle - Pointer to the handle object to be created
    !!
    !! @return Error code indicating the operation's success
    !!
    !! @see multio_base_handle_new
    !! @see multio_base_handle_delete
    !!
    function multio_base_handle_new_default(handle) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_base_handle), intent(inout) :: handle
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_new_handle_default(handle) result(err) &
                    bind(c, name='multio_new_handle')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), intent(out) :: handle
                integer(c_int) :: err
            end function c_multio_new_handle_default
        end interface
        ! implementation
        c_err = c_multio_new_handle_default(handle%impl)
        ! Setting return value
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_base_handle_new_default


    !> @brief Delete a multio handle.
    !!
    !! This function deallocates all the allocated memory and removes any installed
    !! error handler associated with the provided handle object.
    !!
    !! @param [in,out] handle - Pointer to the handle object to be deleted
    !!
    !! @return Error code indicating the operation's success
    !!
    !! @see multio_base_handle_new
    !! @see multio_base_handle_new_default
    !!
    function multio_base_handle_delete(handle) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        ! Variable references from the project
        use :: multio_api_error_handling_mod, only: failure_info_list
        use :: multio_api_constants_mod,      only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_base_handle), intent(inout) :: handle
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variabels
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_delete_handle(handle) result(err) &
                    bind(c, name='multio_delete_handle')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: handle
                integer(c_int) :: err
            end function c_multio_delete_handle
        end interface
        ! Implementation
        c_err = c_multio_delete_handle(handle%c_ptr())
        handle%impl = c_null_ptr
        ! Remove failure handler
        if(associated(handle%failure_id)) then
           call failure_info_list%remove(handle%failure_id)
           handle%failure_id => null()
        end if
        ! Setting return value
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_base_handle_delete


    !> @brief Set the failure handler for multio.
    !!
    !! This function sets the provided handler function to be called as the failure handler
    !! for the specified configuration context.
    !!
    !! @param [in,out] handle  - Pointer to the handle object
    !! @param [in]     handler - Handler function to be called as the failure handler
    !! @param [in,out] context - Configuration context
    !!
    !! @return Error code indicating the operation's success
    !!
    function multio_base_handle_set_failure_handler( handle, handler, context) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_fortran_env, only: int64
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_null_ptr
        use, intrinsic :: iso_c_binding, only: c_funloc
        use, intrinsic :: iso_c_binding, only: c_f_pointer
        ! Variable references from the project
        use :: multio_api_error_handling_mod, only: failure_handler_t
        use :: multio_api_error_handling_mod, only: failure_info_list
        use :: multio_api_constants_mod,      only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_base_handle),    intent(inout) :: handle
        procedure(failure_handler_t)                :: handler
        integer(int64),               intent(inout) :: context
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variabels
        integer(kind=c_int) :: c_err
        type(c_ptr) :: new_id_loc
        integer(c_int), pointer :: old_id
        ! Private interface to the c API
        interface
            function c_multio_handle_set_failure_handler(mio, handler, context) result(err) &
                bind(c, name='multio_handle_set_failure_handler')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_funptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: mio
                type(c_funptr), value, intent(in) :: handler
                type(c_ptr),    value, intent(in) :: context
                integer(c_int) :: err
            end function c_multio_handle_set_failure_handler
        end interface
        ! Initialization
        new_id_loc = c_null_ptr
        old_id => null()
        ! Search for old error handlers
        if(associated(handle%failure_id)) then
            old_id => handle%failure_id
        endif
        ! Append the new error handler
        new_id_loc = failure_info_list%add(handler, context)
        call c_f_pointer(new_id_loc, handle%failure_id)
        c_err = c_multio_handle_set_failure_handler(handle%c_ptr(), failure_info_list%c_wrapper(), new_id_loc)
        ! Revo the old error handler if exists
        if(associated(old_id)) then
            call failure_info_list%remove(old_id)
        endif
        ! Setting return value
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_base_handle_set_failure_handler


    !> @brief Open a new connection.
    !!
    !! This function initiates the process of opening a new
    !! connection using the provided handle object.
    !!
    !! @param [in,out] handle - Pointer to the handle object
    !!
    !! @return Error code indicating the operation's success
    !!
    !! @see multio_base_handle_close_connections
    !!
    function multio_base_handle_open_connections(handle) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_base_handle), intent(inout) :: handle
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_open_connections(handle) result(err) &
                    bind(c, name='multio_open_connections')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: handle
                integer(c_int) :: err
            end function c_multio_open_connections
        end interface
        ! implementation
        c_err = c_multio_open_connections(handle%c_ptr())
        ! Setting return value
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_base_handle_open_connections


    !> @brief Close any existing connection.
    !!
    !! This function initiates the process of closing any existing
    !! connection using the provided handle object.
    !!
    !! @param [in,out] handle - Pointer to the handle object
    !!
    !! @return Error code indicating the operation's success
    !!
    !! @see multio_base_handle_open_connections
    !!
    function multio_base_handle_close_connections(handle) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_base_handle), intent(inout) :: handle
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_close_connections(handle) result(err) &
                    bind(c, name='multio_close_connections')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: handle
                integer(c_int) :: err
            end function c_multio_close_connections
        end interface
        ! implementation
        c_err = c_multio_close_connections(handle%c_ptr())
        ! Setting return value
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_base_handle_close_connections

end module multio_api_base_handle_mod
