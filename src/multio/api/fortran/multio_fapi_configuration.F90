!> @file
!!
!! @brief Definition of an object that can be utilized as the primary
!!        configuration for a multio_handle
!!

module multio_api_configuration_mod

    use, intrinsic :: iso_c_binding, only: c_int
    use, intrinsic :: iso_c_binding, only: c_ptr
    use, intrinsic :: iso_c_binding, only: c_null_ptr

implicit none

    ! Default visibility of the module
    private

    !>
    !! @class datatype used to wrap the functionalities of a multio_configuration object
    type :: multio_configuration

        !! Deafult visibility of the members
        private

        !! Pointer to the opaque c object
        type(c_ptr) :: impl = c_null_ptr

        !! Pointer to the failure handler
        integer(c_int), pointer :: failure_id => null()

    contains
        procedure, public, pass :: new_default       => multio_new_configuration
        procedure, public, pass :: new_from_filename => multio_new_configuration_from_filename
        procedure, public, pass :: c_ptr             => multio_configuration_c_ptr
        generic,   public       :: new => new_default, new_from_filename

        procedure, public, pass :: delete                       => multio_delete_configuration
        procedure, public, pass :: set_failure_handler          => multio_config_set_failure_handler

        procedure, public, pass :: move_failure_id              => multio_config_move_failure_id
        procedure, public, pass :: set_path                     => multio_config_set_path

        ! Add overload for backward compatibility with logical(kind=c_bool) argument
        procedure, private, pass :: mpi_allow_world_default_comm_logical => multio_mpi_allow_world_default_comm_logical
        procedure, private, pass :: mpi_allow_world_default_comm_cbool   => multio_mpi_allow_world_default_comm_cbool
        generic,   public :: mpi_allow_world_default_comm       => mpi_allow_world_default_comm_logical, &
                                                                &  mpi_allow_world_default_comm_cbool

        procedure, public, pass :: mpi_parent_comm              => multio_mpi_parent_comm
        procedure, public, pass :: mpi_return_client_comm       => multio_mpi_return_client_comm
        procedure, public, pass :: mpi_return_server_comm       => multio_mpi_return_server_comm
    end type ! multio_configuration

    ! Public symbols whitelist
    public :: multio_configuration

contains

    !> @brief Retrieve the failure ID from the given configuration context.
    !!
    !! This subroutine retrieves the failure ID associated with the provided
    !! configuration context handle.
    !!
    !! @param[in,out] cc - A handle to the configuration context object.
    !!
    !! @return A pointer to the failure ID associated with the context.
    !!         If no failure ID is available, a null pointer is returned.
    !!
    function multio_config_move_failure_id( cc ) result(pfid)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Dummy arguments
        class(multio_configuration), target, intent(inout) :: cc
        ! Function result
        integer(c_int), pointer :: pfid
#if !defined(MULTIO_DUMMY_API)
        ! Implementation
        pfid => cc%failure_id
        cc%failure_id => null()
#else
        pfid => null()
#endif
        ! Exit point
        return
    end function multio_config_move_failure_id


    !> @brief Extract the C pointer of the configuration object.
    !!
    !! This subroutine extracts and returns the C pointer associated with the
    !! configuration object handled by the provided configuration context.
    !!
    !! @param[in,out] cc - A handle to the configuration context object.
    !!
    !! @return A C pointer to the configuration object.
    !!         If the object is not available, NULL pointer is returned.
    !!
    function multio_configuration_c_ptr( cc ) result(loc)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_ptr
        use, intrinsic :: iso_c_binding, only: c_null_ptr
    implicit none
        ! Dummy arguments
        class(multio_configuration), target, intent(inout) :: cc
        ! Function result
        type(c_ptr) :: loc
#if !defined(MULTIO_DUMMY_API)
        ! Implementation
        loc = cc%impl
#else
        loc = c_null_ptr
#endif
        ! Exit point
        return
    end function multio_configuration_c_ptr


    !> @brief Create a new configuration object.
    !!
    !! This subroutine creates a new configuration object and returns an error code
    !! indicating the operation's success.
    !!
    !! @param[in,out] cc - A handle passed as an object pointer.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_new_configuration_from_filename
    !! @see multio_delete_configuration
    !!
    function multio_new_configuration(cc) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_configuration), intent(out) :: cc
        ! Function Result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_new_configuration(cc) result(err) &
                bind(c, name='multio_new_configuration')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), intent(out) :: cc
                integer(c_int) :: err
            end function c_multio_new_configuration
        end interface
        ! Call the c API
        c_err = c_multio_new_configuration(cc%impl)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_new_configuration


    !> @brief Create a new configuration object from a file name.
    !!
    !! This subroutine creates a new configuration object using the provided file name
    !! and returns an error code indicating the operation's success.
    !!
    !! @param[in,out] cc - A handle passed as an object pointer.
    !! @param[in] file_name - The name of the file for configuration.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_new_configuration
    !! @see multio_delete_configuration
    !!
    function multio_new_configuration_from_filename(cc, file_name) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_char
        use, intrinsic :: iso_c_binding, only: c_null_char
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_configuration), intent(inout) :: cc
        character(len=*),            intent(in)    :: file_name
        ! Function Result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        character(:,kind=c_char), allocatable, target :: nullified_path
        ! Private interface to the c API
        interface
            function c_multio_new_configuration_from_filename(cc, file_name) result(err) &
                bind(c, name='multio_new_configuration_from_filename')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), intent(in), value :: file_name
                type(c_ptr), intent(out) :: cc
                integer(c_int) :: err
            end function c_multio_new_configuration_from_filename
        end interface
        ! Initialization and allocation
        nullified_path = trim(file_name) // c_null_char
        ! Call the c API
        c_err = c_multio_new_configuration_from_filename(cc%impl, c_loc(nullified_path))
        ! Output cast and cleanup
        if (allocated(nullified_path)) deallocate(nullified_path)
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_new_configuration_from_filename


    !> @brief Delete a configuration object.
    !!
    !! This subroutine deletes the specified configuration object referenced by the provided handle
    !! and returns an error code indicating the operation's success.
    !!
    !! @param[in,out] cc - A handle passed as an object pointer.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_new_configuration
    !! @see multio_new_configuration_from_filename
    !!
    function multio_delete_configuration(cc) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_configuration), intent(inout) :: cc
        ! Function Result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_delete_configuration(cc) result(err) &
                bind(c, name='multio_delete_configuration')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), intent(in), value :: cc
                integer(c_int) :: err
            end function c_multio_delete_configuration
        end interface
        ! Call the c API
        c_err = c_multio_delete_configuration(cc%impl)
        cc%impl = c_null_ptr
        ! Output cast and cleanup
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_delete_configuration


    !> @brief Set the failure handler to multio.
    !!
    !! This subroutine sets the provided handler function to be called as the failure handler
    !! for the specified configuration context and returns an error code indicating the operation's success.
    !!
    !! @param [in,out] cc      - Pointer to the multio_configuration
    !!                          object used to configure the handle
    !! @param [in] handler     - The handler function to be called as the failure handler.
    !! @param [in,out] context - The configuration context.
    !!
    !! @return An error code indicating the operation's success.
    !!
    function multio_config_set_failure_handler( cc, handler, context) result(err)
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
        class(multio_configuration),           intent(inout) :: cc
        procedure(failure_handler_t), pointer, intent(in)    :: handler
        integer(int64),                        intent(inout) :: context
        ! Function result
        integer :: err


#if !defined(MULTIO_DUMMY_API)
        ! Local variabels
        integer(kind=c_int) :: c_err
        type(c_ptr) :: new_id_loc
        integer(c_int), pointer :: old_id
        ! Private interface to the c API
        interface
            function c_multio_config_set_failure_handler(mio, handler, context) result(err) &
                bind(c, name='multio_config_set_failure_handler')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_funptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: mio
                type(c_funptr), value, intent(in) :: handler
                type(c_ptr),    value, intent(in) :: context
                integer(c_int) :: err
            end function c_multio_config_set_failure_handler
        end interface
        ! Initialization
        new_id_loc = c_null_ptr
        old_id => null()
        ! Search for old error handlers
        if(associated(cc%failure_id)) then
            old_id => cc%failure_id
        endif
        ! Append the new error handler
        new_id_loc = failure_info_list%add(handler, context)
        call c_f_pointer(new_id_loc, cc%failure_id)
        c_err = c_multio_config_set_failure_handler(cc%c_ptr(), failure_info_list%c_wrapper(), new_id_loc)
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
    end function multio_config_set_failure_handler


    !> @brief Set the path for the configuration.
    !!
    !! This subroutine sets the provided path as the path for the configuration
    !! associated with the given configuration context and returns an error code
    !! indicating the operation's success.
    !!
    !! @param[in,out] cc - A handle passed as an object pointer.
    !! @param[in] path - The path for the configuration.
    !!
    !! @return An error code indicating the operation's success.
    !!
    function multio_config_set_path(cc, path) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_char
        use, intrinsic :: iso_c_binding, only: c_null_char
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_configuration), intent(inout) :: cc
        character(len=*),            intent(in)    :: path
        ! Function Result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        character(:,kind=c_char), allocatable, target :: nullified_path
        ! Private interface to the c API
        interface
            function c_multio_config_set_path(cc, path) result(err) &
                bind(c, name='multio_config_set_path')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), intent(in), value :: path
                type(c_ptr), intent(in), value :: cc
                integer(c_int) :: err
            end function c_multio_config_set_path
        end interface
        ! Initialization and allocation
        nullified_path = trim(path) // c_null_char
        ! Call the c API
        c_err = c_multio_config_set_path(cc%impl, c_loc(nullified_path))
        ! Output cast and cleanup
        if (allocated(nullified_path)) deallocate(nullified_path)
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_config_set_path


    !> @brief Allow `MPI_COMM_WORLD` as the default communicator.
    !!
    !! This subroutine sets a flag to allow the use of `MPI_COMM_WORLD` as the default communicator
    !! for the operations associated with the given configuration context. It returns an error code
    !! indicating the operation's success.
    !!
    !! @param[in,out] cc - A handle passed as an object pointer.
    !! @param[in] allow - A flag indicating whether to allow the use of `MPI_COMM_WORLD`.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_mpi_return_server_comm
    !! @see multio_mpi_return_client_comm
    !! @see multio_mpi_parent_comm
    !! @see multio_mpi_client_id
    !!
    function multio_mpi_allow_world_default_comm_logical(cc, allow) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_bool
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_configuration), intent(inout) :: cc
        logical,                     intent(in)    :: allow
        ! Function Result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        logical(c_bool) :: c_allow
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_mpi_allow_world_default_comm(cc, allow) result(err) &
                bind(c, name='multio_mpi_allow_world_default_comm')
                use, intrinsic :: iso_c_binding, only: c_bool
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),     value, intent(in) :: cc
                logical(c_bool), value, intent(in) :: allow
                integer(c_int) :: err
            end function c_multio_mpi_allow_world_default_comm
        end interface
        ! Initialization and allocation
        c_allow = logical(allow,c_bool)
        ! Call the c API
        c_err = c_multio_mpi_allow_world_default_comm(cc%impl, c_allow)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_mpi_allow_world_default_comm_logical

    function multio_mpi_allow_world_default_comm_cbool(cc, allow) result(err)
    use, intrinsic :: iso_c_binding, only : c_bool
    implicit none
        class(multio_configuration), intent(inout) :: cc
        logical(kind=c_bool),        intent(in)    :: allow
        ! Function Result
        integer :: err
        err = multio_mpi_allow_world_default_comm_logical(cc, logical(allow))
    end function multio_mpi_allow_world_default_comm_cbool

    !> @brief Set the MPI parent communicator.
    !!
    !! This subroutine sets the provided parent communicator as the MPI parent communicator
    !! for the operations associated with the given configuration context.
    !! It returns an error code indicating the operation's success.
    !!
    !! @param[in,out] cc - A handle passed as an object pointer.
    !! @param[in] parent_comm - The MPI parent communicator.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_mpi_return_server_comm
    !! @see multio_mpi_return_client_comm
    !! @see multio_mpi_client_id
    !! @see multio_mpi_allow_world_default_comm
    !!
    function multio_mpi_parent_comm(cc, parent_comm) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_configuration), intent(inout) :: cc
        integer(c_int),              intent(in)    :: parent_comm
        ! Function Result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_mpi_parent_comm(cc, parent_comm) result(err) &
                bind(c, name='multio_mpi_parent_comm')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: cc
                integer(c_int), value, intent(in) :: parent_comm
                integer(c_int) :: err
            end function c_multio_mpi_parent_comm
        end interface
        ! Call the c API
        c_err = c_multio_mpi_parent_comm(cc%impl, parent_comm)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_mpi_parent_comm


    !> @brief Set the MPI client communicator.
    !!
    !! This subroutine returns the communicator MPI client communicator
    !! for the operations associated with the given configuration context.
    !! It returns an error code indicating the operation's success.
    !!
    !! @param[in,out] cc - A handle passed as an object pointer.
    !! @param[out] return_comm - The return communicator.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_mpi_return_server_comm
    !! @see multio_mpi_parent_comm
    !! @see multio_mpi_client_id
    !! @see multio_mpi_allow_world_default_comm
    !!
    function multio_mpi_return_client_comm(cc, return_comm) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_configuration), intent(inout) :: cc
        integer(c_int),              intent(out)   :: return_comm ! can be c_null_ptr
        ! Function Result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_mpi_return_client_comm(cc, return_comm) result(err) &
                bind(c, name='multio_mpi_return_client_comm')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in)  :: cc
                integer(c_int),     intent(out) :: return_comm ! can be c_null_ptr
                integer(c_int) :: err
            end function c_multio_mpi_return_client_comm
        end interface
        ! Call the c API
        c_err = c_multio_mpi_return_client_comm(cc%impl, return_comm)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
#else
        return_comm = int(0,c_int)
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_mpi_return_client_comm


    !> @brief Set the MPI server communicator.
    !!
    !! This subroutine returns the MPI server communicator
    !! for the operations associated with the given configuration context.
    !! It returns an error code indicating the operation's success.
    !!
    !! @param[in,out] cc - A handle passed as an object pointer.
    !! @param[out] return_comm - The return communicator.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_mpi_return_client_comm
    !! @see multio_mpi_parent_comm
    !! @see multio_mpi_client_id
    !! @see multio_mpi_allow_world_default_comm
    !!
    function multio_mpi_return_server_comm(cc, return_comm) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_configuration), intent(inout) :: cc
        integer(c_int), intent(out) :: return_comm ! can be c_null_ptr
        ! Function Result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_mpi_return_server_comm(cc, return_comm) result(err) &
                bind(c, name='multio_mpi_return_server_comm')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in)  :: cc
                integer(c_int),     intent(out) :: return_comm ! can be c_null_ptr
                integer(c_int) :: err
            end function c_multio_mpi_return_server_comm
        end interface
        ! Call the c API
        c_err = c_multio_mpi_return_server_comm(cc%impl, return_comm)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
#else
        return_comm = int(0,c_int)
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_mpi_return_server_comm

end module multio_api_configuration_mod
