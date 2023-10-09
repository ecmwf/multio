!> @file
!!
!! @brief Definition of the failure handlers.
!!
!! Failure handlers are Fortran functions that need to be called from
!! the "c" lower level.
!!
!! This module has 2 main objectives:
!!
!! 1) Avoid forcing the users of the API to define handlers that are
!!    callable from "c". This is achieved by wrapping Fortran failure
!!    handlers with a c-callable wrapper.
!! 2) Allow different failure handlers for different situations. This
!!    is achieved through a list of failure handlers. The specific failure
!!    handler to be called is then identified in the list using a unique index.
!!
!! @todo: In order to enforce the usage of the provided interface, it might be
!!        useful to accept only procedure pointers instead of procedures.
!!

module multio_api_error_handling_mod

    use, intrinsic :: iso_c_binding,   only: c_int
    use, intrinsic :: iso_c_binding,   only: c_ptr
    use, intrinsic :: iso_c_binding,   only: c_null_ptr
    use, intrinsic :: iso_fortran_env, only: int64

implicit none

    ! Default visibility
    private


    ! Error handling definitions
    type :: multio_failure_info
        type(c_ptr) :: impl = c_null_ptr
    end type


    !>
    !! Interface of the failure handlers
    !! All failure handlers set from fortran codes need to have this interface
    abstract interface
        subroutine failure_handler_t(context, error, info)
            use, intrinsic :: iso_fortran_env, only: int64
            import :: multio_failure_info
        implicit none
            integer(int64),             intent(inout) :: context
            integer,                    intent(in)    :: error
            class(multio_failure_info), intent(in)    :: info
        end subroutine failure_handler_t
    end interface


    !>
    !! node used for storing the failure handlers
    type :: multio_fort_failure_info_node
        integer(c_int) :: id = 0
        integer(int64) :: context = 0
        procedure(failure_handler_t), nopass, pointer :: handler_fn => null()
        type(multio_fort_failure_info_node),  pointer :: next => null()
    end type


    !>
    !! list of failure handlers
    type :: multio_fort_failure_info_list

        ! Default visibility of the members
        private

        integer(c_int) :: lastId = 0
        integer :: count = 0
        type(multio_fort_failure_info_node), pointer :: head => null()
        type(multio_fort_failure_info_node), pointer :: tail => null()

    contains

        ! Lis management
        procedure, public, pass :: callHandler => multio_fort_failure_call
        procedure, public, pass :: add         => multio_fort_failure_add
        procedure, public, pass :: remove      => multio_fort_failure_remove

        ! Address of the procedure needed to call an error handler
        procedure, public, pass :: c_wrapper   => multio_fort_failure_wrapper_addr

    end type


    !> Lis of handlers
    type(multio_fort_failure_info_list), save :: failure_info_list


    ! Whitelist of public symbols
    public :: failure_info_list

    public :: failure_handler_t
    public :: multio_failure_info

contains


    !> @brief Return the pointer to the C wrapper that needs to be called from C.
    !!
    !! This function retrieves the C wrapper's address that should be called from the C level.
    !!
    !! @param [in] ffi   The list of failure handlers.
    !!
    !! @return The C address of the wrapper.
    !!
    function multio_fort_failure_wrapper_addr( ffi ) result(c_addr)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_funptr
        use, intrinsic :: iso_c_binding, only: c_funloc
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_fort_failure_info_list), intent(inout) :: ffi
        ! Function Result
        type(c_funptr) :: c_addr
        ! Implementation
        c_addr = c_funloc(failure_handler_wrapper)
        ! Exit point
        return
    end function multio_fort_failure_wrapper_addr


    !> @brief Call the failure handler associated with the given ID.
    !!
    !! This function calls the failure handler associated with the provided ID.
    !!
    !! @param [in,out] ffi   The list of failure handlers.
    !! @param [in]     id    The ID of the failure handler to be called.
    !! @param [in]     err   The error ID during the call of the error handler.
    !! @param [in]     info  The failure info.
    !!
    subroutine multio_fort_failure_call(ffi, id, err, info)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Dummy arguments
        class(multio_fort_failure_info_list), intent(inout) :: ffi
        integer(c_int),                       intent(in)    :: id
        integer,                              intent(in)    :: err
        class(multio_failure_info),           intent(in)    :: info
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        type(multio_fort_failure_info_node), pointer :: node
        ! Implementation

        node => ffi%head
        SearchLoop: do while(associated(node))
            if (node%id .eq. id) then
                !! Call the failure handler
                call node%handler_fn(node%context, err, info)
                node => null()
            else
                !! Go to the next node
                node => node%next
            endif
        enddo SearchLoop
#endif
        ! Exit point
        return
    end subroutine multio_fort_failure_call


    !> @brief Add a failure handler to the list of failure handlers.
    !!
    !! This function adds a failure handler along with its associated context to the list
    !! of failure handlers.
    !!
    !! @param [in,out] ffi         The list of failure handlers.
    !! @param [in]     handler_fn  The failure handler to be added to the list.
    !! @param [in]     context     The context associated with the failure handler.
    !!
    !! @return c pointer to the node that constains the error handler
    !!
    function multio_fort_failure_add(ffi, handler_fn, context) result(new_id_loc)

        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding,   only: c_loc
        use, intrinsic :: iso_c_binding,   only: c_ptr
        use, intrinsic :: iso_c_binding,   only: c_null_ptr
        use, intrinsic :: iso_fortran_env, only: int64

    implicit none
        ! Dummy arguments
        class(multio_fort_failure_info_list),  intent(inout) :: ffi
        procedure(failure_handler_t)                         :: handler_fn
        integer(int64),                        intent(in)    :: context
        ! Function result
        type(c_ptr) :: new_id_loc
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        class(multio_fort_failure_info_node), pointer :: new_node

        ! Update the counters in the list
        ffi%lastId = ffi%lastId + 1
        ffi%count = ffi%count + 1

        ! Allocate the new node
        allocate(new_node)
        new_node%id = ffi%lastId
        new_node%handler_fn => handler_fn
        new_node%context = context

        ! Get the c_pointer of the id of the current node
        new_id_loc = c_loc(new_node%id)

        ! Special case: List empty
        if(.not. associated(ffi%head)) then
            ffi%head => new_node
        end if

        ! Add the node to the end of the list
        if(associated(ffi%tail)) then
            ffi%tail%next => new_node
        endif
        ffi%tail => new_node
#else
        new_id_loc = c_null_ptr
#endif
        ! Exit point
        return
    end function multio_fort_failure_add


    !> @brief Remove a failure handler from the list of failure handlers.
    !!
    !! This function removes a failure handler identified by its index ("id") from the list
    !! of failure handlers.
    !!
    !! @param [in,out] ffi  The list of failure handlers.
    !! @param [in]     id   The index of the failure handler to be removed.
    !!
    subroutine multio_fort_failure_remove(ffi, id)

        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int

    implicit none
        ! Dummy arguments
        class(multio_fort_failure_info_list), intent(inout) :: ffi
        integer(c_int),                       intent(in)    :: id
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        type(multio_fort_failure_info_node), pointer :: node
        type(multio_fort_failure_info_node), pointer :: node_prev
        ! Initialization
        node_prev => null()
        node => ffi%head
        ! Implementation
        SearchLoop: do while (associated(node))

            if (node%id == id) then

                !! Node found: remove node from the list

                ! Generic case: the node to be removed is a genric node inside the list
                if(associated(node_prev)) then
                    node_prev%next => node%next
                end if

                ! Special case: the node to be removed is the head
                if(associated(ffi%head, node)) then
                    ffi%head => node%next
                endif

                ! Special case: the node to be removed is the tail
                if(associated(ffi%tail, node)) then
                    ffi%tail => node_prev
                endif

                ! Update node counter
                ffi%count = ffi%count - 1

                ! Free node
                deallocate(node)
                node => null()
            else

                !! Node not found: go to the next node
                node_prev => node
                node => node%next

            endif

        enddo SearchLoop
#endif
        ! Exit point
        return
    end subroutine multio_fort_failure_remove


    !> @brief Wrap a failure handler.
    !!
    !! This function serves as the entry point for the failure handler when called from C.
    !! The linker symbol associated with this function is defined using the `bind` keyword.
    !!
    !! @param [in] c_pidx   Identifier of the failure handler to be called.
    !! @param [in] c_error  Identifier of the error.
    !! @param [in] c_info   Opaque information about the error.
    !!
    subroutine failure_handler_wrapper(c_pidx, c_error, c_info) &
                bind(c, name='failure_handler_wrapper')

        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_ptr
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_f_pointer

    implicit none
        ! Dummy arguments
        type(c_ptr),    value, intent(in) :: c_pidx
        integer(c_int), value, intent(in) :: c_error
        type(c_ptr),    value, intent(in) :: c_info
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        type(multio_failure_info) :: finfo
        integer(c_int), pointer :: c_idx
        integer :: err
        ! Convert c input data to fortran data
        call c_f_pointer( c_pidx, c_idx )
        finfo%impl = c_info
        err = int(c_error,kind(err))
        ! Call the fortran failure handler
        call failure_info_list%callHandler(c_idx, err, finfo)
#endif
        ! Exit point
        return
    end subroutine failure_handler_wrapper

end module multio_api_error_handling_mod
