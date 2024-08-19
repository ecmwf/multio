!> @file
!!
!! @brief Definition of the main functionalities of a multio_handle.
!!
!! This module defines all the overloaded methods needed to write
!! multio fields, masks, and domains.
!!
!! @note This module is separated from the "base" multio handle in
!!       order to avoid circular dependencies.
!!

module multio_api_handle_mod

    use, intrinsic :: iso_c_binding,   only: c_ptr
    use, intrinsic :: iso_c_binding,   only: c_int
    use, intrinsic :: iso_c_binding,   only: c_null_ptr
    use :: multio_api_base_handle_mod, only: multio_base_handle

implicit none

    ! Default symbols visibility
    private

    !>
    !! @class datatype used to wrap the functionalities of a multio_handle object
    type, extends(multio_base_handle) :: multio_handle

        !! Deafult visibility of the members
        private

    contains

        ! Signalling
        procedure, public,  pass :: flush                => multio_handle_flush
        procedure, public,  pass :: notify               => multio_handle_notify

        ! Domain handling
        procedure, public,  pass :: write_domain         => multio_handle_write_domain

        ! Parametrization handling
        procedure, public,  pass :: write_parametrization_metadata => multio_handle_write_parametrization
        procedure, public,  pass :: write_parametrization_array    => multio_handle_write_parametrization_array
        generic,   public        :: write_parametrization          => write_parametrization_array, &
                                                                    & write_parametrization_metadata

        ! Mask handling
        procedure, private, pass :: write_mask_float_1d  => multio_handle_write_mask_float_1d
        procedure, private, pass :: write_mask_double_1d => multio_handle_write_mask_double_1d
        procedure, private, pass :: write_mask_float_2d  => multio_handle_write_mask_float_2d
        procedure, private, pass :: write_mask_double_2d => multio_handle_write_mask_double_2d
        generic,   public        :: write_mask => write_mask_float_1d,  &
                                               &  write_mask_float_2d,  &
                                               &  write_mask_double_1d, &
                                               &  write_mask_double_2d

        ! Field handling
        procedure, private, pass :: write_field_buffer    => multio_handle_write_field_buffer
        procedure, private, pass :: write_field_float_1d  => multio_handle_write_field_float_1d
        procedure, private, pass :: write_field_double_1d => multio_handle_write_field_double_1d
        procedure, private, pass :: write_field_float_2d  => multio_handle_write_field_float_2d
        procedure, private, pass :: write_field_double_2d => multio_handle_write_field_double_2d
        generic,   public        :: write_field => write_field_float_1d,  &
                                                &  write_field_float_2d,  &
                                                &  write_field_double_1d, &
                                                &  write_field_double_2d, &
                                                   write_field_buffer

        procedure, public, pass :: write_grib_encoded => multio_handle_write_grib_encoded


        ! Utils
        procedure, public,  pass :: field_accepted => multio_handle_field_accepted

    end type ! multio_handle

    ! Public symbols whitelist
    public :: multio_handle

contains

    !> @brief Send a flush command through the multio plans.
    !!
    !! This function sends a flush command through the multio plans of the object
    !! pointed to by the provided handle.
    !!
    !! @param [in,out] handle     A pointer to the object handle.
    !! @param [in]     metadata   Metadata to be sent with the flush command.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_handle_notify
    !!
    function multio_handle_flush(handle, metadata) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        ! Variable references from the project
        use :: multio_api_metadata_mod,  only: multio_metadata
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_handle),   intent(inout) :: handle
        class(multio_metadata), intent(inout) :: metadata
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_flush(handle, metadata) result(err) &
                bind(c, name='multio_flush')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: handle
                type(c_ptr), value, intent(in) :: metadata
                integer(c_int) :: err
            end function c_multio_flush
        end interface
        ! Implementation
        c_err = c_multio_flush(handle%c_ptr(), metadata%c_ptr())
        ! Setting return value
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_handle_flush


    !> @brief Notify an event through the multio plans.
    !!
    !! This function notifies an event through the multio plans of the object
    !! pointed to by the provided handle.
    !!
    !! @param [in,out] handle     A pointer to the object handle.
    !! @param [in]     metadata   Metadata to be sent with the notification.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_handle_flush
    !!
    function multio_handle_notify(handle, metadata) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        ! Variable references from the project
        use :: multio_api_metadata_mod,  only: multio_metadata
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_handle),   intent(inout) :: handle
        class(multio_metadata), intent(inout) :: metadata
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_notify(handle, metadata) result(err) &
                    bind(c, name='multio_notify')
                    use, intrinsic :: iso_c_binding, only: c_ptr
                    use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: handle
                type(c_ptr), value, intent(in) :: metadata
                integer(c_int) :: err
            end function c_multio_notify
        end interface
        ! Implementation
        c_err = c_multio_notify(handle%c_ptr(), metadata%c_ptr())
        ! Setting return value
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_handle_notify


    !> @brief Send domain information to multio.
    !!
    !! This function sends domain information to the multio of the object
    !! pointed to by the provided handle.
    !!
    !! @param [in,out] handle   A pointer to the object handle.
    !! @param [in]     metadata Metadata to be sent with the domain.
    !! @param [in]     data     Domain data.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @todo Implement for different kinds.
    !!
    function multio_handle_write_domain(handle, metadata, data) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        ! Variable references from the project
        use :: multio_api_metadata_mod,  only: multio_metadata
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_handle),           intent(inout) :: handle
        class(multio_metadata),         intent(inout) :: metadata
        integer, dimension(:),  target, intent(in)    :: data
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: c_size
        ! Private interface to the c API
        interface
            function c_multio_write_domain(handle, metadata, data, size) result(err) &
                    bind(c, name='multio_write_domain')
                    use, intrinsic :: iso_c_binding, only: c_ptr
                    use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: handle
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: data
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_write_domain
        end interface
        ! Implementation
        c_size = int(size(data),c_int)
        c_err  = c_multio_write_domain(handle%c_ptr(), metadata%c_ptr(), c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_handle_write_domain


    !> @brief Send parametrization information to multio.
    !!
    !! This function sends parametrization information (global & static metadata) to the multio of the object
    !! pointed to by the provided handle.
    !!
    !! @param [in,out] handle   A pointer to the object handle.
    !! @param [in]     metadata Metadata to be sent with the parametrization.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @todo Implement for different kinds.
    !!
    function multio_handle_write_parametrization(handle, metadata) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        ! Variable references from the project
        use :: multio_api_metadata_mod,  only: multio_metadata
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_handle),           intent(inout) :: handle
        class(multio_metadata),         intent(inout) :: metadata
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_write_parametrization(handle, metadata) result(err) &
                    bind(c, name='multio_write_parametrization')
                    use, intrinsic :: iso_c_binding, only: c_ptr
                    use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: handle
                type(c_ptr),    value, intent(in) :: metadata
                integer(c_int) :: err
            end function c_multio_write_parametrization
        end interface
        ! Implementation
        c_err  = c_multio_write_parametrization(handle%c_ptr(), metadata%c_ptr())
        ! Setting return value
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_handle_write_parametrization


    !> @brief Send parametrization information to multio.
    !!
    !! This function sends parametrization information (global & static metadata) to the multio of the object
    !! pointed to by the provided handle.
    !!
    !! @param [in,out] handle   A pointer to the object handle.
    !! @param [in]     metadata Metadata to be sent with the parametrization_array.
    !! @param [in]     data     Domain data.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @todo Implement for different kinds.
    !!
    function multio_handle_write_parametrization_array(handle, key, data) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_char
        use, intrinsic :: iso_c_binding, only: c_null_char
        use, intrinsic :: iso_c_binding, only: c_loc
        ! Variable references from the project
        use :: multio_api_metadata_mod,  only: multio_metadata
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_handle),           intent(inout) :: handle
        character(len=*),               intent(in)    :: key
        integer, dimension(:),  target, intent(in)    :: data
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: c_size
        character(:,kind=c_char), allocatable, target :: nullified_key
        ! Private interface to the c API
        interface
            function c_multio_write_parametrization_array(handle, key, data, size) result(err) &
                    bind(c, name='multio_write_parametrization_array')
                    use, intrinsic :: iso_c_binding, only: c_ptr
                    use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: handle
                type(c_ptr),    value, intent(in) :: key
                type(c_ptr),    value, intent(in) :: data
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_write_parametrization_array
        end interface
        ! Implementation
        nullified_key = trim(key) // c_null_char
        c_size = int(size(data),c_int)
        c_err  = c_multio_write_parametrization_array(handle%c_ptr(), c_loc(nullified_key), c_loc(data), c_size)
        ! Output cast and cleanup
        if (allocated(nullified_key)) deallocate(nullified_key)
        ! Setting return value
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_handle_write_parametrization_array




    !> @brief Send a one-dimensional float mask.
    !!
    !! This function sends a one-dimensional float mask to the multio of the object
    !! pointed to by the provided handle.
    !!
    !! @param [in,out] handle   A pointer to the object handle.
    !! @param [in]     metadata Metadata to be sent with the mask.
    !! @param [in]     data     Mask data.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_handle_write_mask_float_2d
    !! @see multio_handle_write_mask_double_1d
    !! @see multio_handle_write_mask_double_2d
    !!
    function multio_handle_write_mask_float_1d(handle, metadata, data) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_float
        ! Variable references from the project
        use :: multio_api_metadata_mod,  only: multio_metadata
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_handle),                     intent(inout) :: handle
        class(multio_metadata),                   intent(inout) :: metadata
        real(kind=c_float), dimension(:), target, intent(in)    :: data
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: c_size
        ! Private interface to the c API
        interface
            function c_multio_write_mask_float(handle, metadata, data, size) result(err) &
                bind(c, name='multio_write_mask_float')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: handle
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: data
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_write_mask_float
        end interface
        ! Implementation
        c_size = int(size(data),c_int)
        c_err = c_multio_write_mask_float(handle%c_ptr(), metadata%c_ptr(), c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_handle_write_mask_float_1d


    !> @brief Send a two-dimensional float mask.
    !!
    !! This function sends a two-dimensional float mask to the multio of the object
    !! pointed to by the provided handle.
    !!
    !! @param [in,out] handle   A pointer to the object handle.
    !! @param [in]     metadata Metadata to be sent with the mask.
    !! @param [in]     data     Mask data.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_handle_write_mask_float_1d
    !! @see multio_handle_write_mask_double_1d
    !! @see multio_handle_write_mask_double_2d
    !!
    function multio_handle_write_mask_float_2d(handle, metadata, data) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_float
        ! Variable references from the project
        use :: multio_api_metadata_mod,  only: multio_metadata
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_handle),                       intent(inout) :: handle
        class(multio_metadata),                     intent(inout) :: metadata
        real(kind=c_float), dimension(:,:), target, intent(in)    :: data
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: c_size
        ! Private interface to the c API
        interface
            function c_multio_write_mask_float(handle, metadata, data, size) result(err) &
                bind(c, name='multio_write_mask_float')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: handle
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: data
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_write_mask_float
        end interface
        ! Implementation
        c_size = int(size(data),c_int)
        c_err = c_multio_write_mask_float(handle%c_ptr(), metadata%c_ptr(), c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_handle_write_mask_float_2d


    !> @brief Send a one-dimensional double mask.
    !!
    !! This function sends a one-dimensional double mask to the multio of the object
    !! pointed to by the provided handle.
    !!
    !! @param [in,out] handle   A pointer to the object handle.
    !! @param [in]     metadata Metadata to be sent with the mask.
    !! @param [in]     data     Mask data.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_handle_write_mask_float_1d
    !! @see multio_handle_write_mask_float_2d
    !! @see multio_handle_write_mask_double_2d
    !!
    function multio_handle_write_mask_double_1d(handle, metadata, data) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_double
        ! Variable references from the project
        use :: multio_api_metadata_mod,  only: multio_metadata
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_handle),                      intent(inout) :: handle
        class(multio_metadata),                    intent(inout) :: metadata
        real(kind=c_double), dimension(:), target, intent(in)    :: data
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: c_size
        ! Private interface to the c API
        interface
            function c_multio_write_mask_double(handle, metadata, data, size) result(err) &
                bind(c, name='multio_write_mask_double')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: handle
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: data
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_write_mask_double
        end interface
        ! Implementation
        c_size = int(size(data),c_int)
        c_err = c_multio_write_mask_double(handle%c_ptr(), metadata%c_ptr(), c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_handle_write_mask_double_1d


    !> @brief Send a two-dimensional double mask.
    !!
    !! This function sends a two-dimensional double mask to the multio of the object
    !! pointed to by the provided handle.
    !!
    !! @param [in,out] handle   A pointer to the object handle.
    !! @param [in]     metadata Metadata to be sent with the mask.
    !! @param [in]     data     Mask data.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_handle_write_mask_float_1d
    !! @see multio_handle_write_mask_float_2d
    !! @see multio_handle_write_mask_double_1d
    !!
    function multio_handle_write_mask_double_2d(handle, metadata, data) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_double
        ! Variable references from the project
        use :: multio_api_metadata_mod,  only: multio_metadata
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_handle),                        intent(inout) :: handle
        class(multio_metadata),                      intent(inout) :: metadata
        real(kind=c_double), dimension(:,:), target, intent(in)    :: data
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: c_size
        ! Private interface to the c API
        interface
            function c_multio_write_mask_double(handle, metadata, data, size) result(err) &
                bind(c, name='multio_write_mask_double')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: handle
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: data
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_write_mask_double
        end interface
        ! Implementation
        c_size = int(size(data),c_int)
        c_err = c_multio_write_mask_double(handle%c_ptr(), metadata%c_ptr(), c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_handle_write_mask_double_2d


    !> @brief Send a one-dimensional float field.
    !!
    !! This function sends a one-dimensional float field to the multio of the object
    !! pointed to by the provided handle.
    !!
    !! @param [in,out] handle   A pointer to the object handle.
    !! @param [in]     metadata Metadata to be sent with the field.
    !! @param [in]     data     Field data.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_handle_write_field_float_2d
    !! @see multio_handle_write_field_double_1d
    !! @see multio_handle_write_field_double_2d
    !! @see multio_handle_write_field_buffer
    !!
    function multio_handle_write_field_float_1d(handle, metadata, data) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_float
        ! Variable references from the project
        use :: multio_api_metadata_mod,  only: multio_metadata
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_handle),                     intent(inout) :: handle
        class(multio_metadata),                   intent(inout) :: metadata
        real(kind=c_float), dimension(:), target, intent(in)    :: data
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: c_size
        ! Private interface to the c API
        interface
            function c_multio_write_field_float(handle, metadata, data, size) result(err) &
                bind(c, name='multio_write_field_float')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: handle
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: data
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_write_field_float
        end interface
        ! Implementation
        c_size = int(size(data),c_int)
        c_err = c_multio_write_field_float(handle%c_ptr(), metadata%c_ptr(), c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_handle_write_field_float_1d


    !> @brief Send a two-dimensional float field.
    !!
    !! This function sends a two-dimensional float field to the multio of the object
    !! pointed to by the provided handle.
    !!
    !! @param [in,out] handle   A pointer to the object handle.
    !! @param [in]     metadata Metadata to be sent with the field.
    !! @param [in]     data     Field data.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_handle_write_field_float_1d
    !! @see multio_handle_write_field_double_1d
    !! @see multio_handle_write_field_double_2d
    !! @see multio_handle_write_field_buffer
    !!
    function multio_handle_write_field_float_2d(handle, metadata, data) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_float
        ! Variable references from the project
        use :: multio_api_metadata_mod,  only: multio_metadata
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_handle),                       intent(inout) :: handle
        class(multio_metadata),                     intent(inout) :: metadata
        real(kind=c_float), dimension(:,:), target, intent(in)    :: data
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: c_size
        ! Private interface to the c API
        interface
            function c_multio_write_field_float(handle, metadata, data, size) result(err) &
                bind(c, name='multio_write_field_float')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: handle
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: data
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_write_field_float
        end interface
        ! Implementation
        c_size = int(size(data),c_int)
        c_err = c_multio_write_field_float(handle%c_ptr(), metadata%c_ptr(), c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_handle_write_field_float_2d


    !> @brief Send a one-dimensional double field.
    !!
    !! This function sends a one-dimensional double field to the multio of the object
    !! pointed to by the provided handle.
    !!
    !! @param [in,out] handle   A pointer to the object handle.
    !! @param [in]     metadata Metadata to be sent with the field.
    !! @param [in]     data     Field data.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_handle_write_field_float_1d
    !! @see multio_handle_write_field_float_2d
    !! @see multio_handle_write_field_double_2d
    !! @see multio_handle_write_field_buffer
    !!
    function multio_handle_write_field_double_1d(handle, metadata, data) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_double
        ! Variable references from the project
        use :: multio_api_metadata_mod,  only: multio_metadata
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_handle),                      intent(inout) :: handle
        class(multio_metadata),                    intent(inout) :: metadata
        real(kind=c_double), dimension(:), target, intent(in)    :: data
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: c_size
        ! Private interface to the c API
        interface
            function c_multio_write_field_double(handle, metadata, data, size) result(err) &
                bind(c, name='multio_write_field_double')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: handle
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: data
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_write_field_double
        end interface
        ! Implementation
        c_size = int(size(data),c_int)
        c_err = c_multio_write_field_double(handle%c_ptr(), metadata%c_ptr(), c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_handle_write_field_double_1d


    !> @brief Send a two-dimensional double field.
    !!
    !! This function sends a two-dimensional double field to the multio of the object
    !! pointed to by the provided handle.
    !!
    !! @param [in,out] handle   A pointer to the object handle.
    !! @param [in]     metadata Metadata to be sent with the field.
    !! @param [in]     data     Field data.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_handle_write_field_float_1d
    !! @see multio_handle_write_field_float_2d
    !! @see multio_handle_write_field_double_1d
    !! @see multio_handle_write_field_buffer
    !!
    function multio_handle_write_field_double_2d(handle, metadata, data) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_double
        ! Variable references from the project
        use :: multio_api_metadata_mod,  only: multio_metadata
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_handle),                        intent(inout) :: handle
        class(multio_metadata),                      intent(inout) :: metadata
        real(kind=c_double), dimension(:,:), target, intent(in)    :: data
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: c_size
        ! Private interface to the c API
        interface
            function c_multio_write_field_double(handle, metadata, data, size) result(err) &
                bind(c, name='multio_write_field_double')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: handle
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: data
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_write_field_double
        end interface
        ! Implementation
        c_size = int(size(data),c_int)
        c_err = c_multio_write_field_double(handle%c_ptr(), metadata%c_ptr(), c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_handle_write_field_double_2d


    !> @brief Send a buffered field (data is already packed in an eckit::buffer).
    !!
    !! This function sends a buffered field (data that is already packed in an eckit::buffer)
    !! to the multio of the object pointed to by the provided handle.
    !!
    !! @param [in,out] handle   A pointer to the object handle.
    !! @param [in]     metadata Metadata to be sent with the field.
    !! @param [in]     data     Field data.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_handle_write_field_float_1d
    !! @see multio_handle_write_field_float_2d
    !! @see multio_handle_write_field_double_1d
    !! @see multio_handle_write_field_double_2d
    !!
    function multio_handle_write_field_buffer(handle, metadata, data) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        ! Variable references from the project
        use :: multio_api_metadata_mod,  only: multio_metadata
        use :: multio_api_data_mod,      only: multio_data
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_handle),       intent(inout) :: handle
        class(multio_metadata),     intent(inout) :: metadata
        class(multio_data), target, intent(inout) :: data
        ! Function rsult
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) c_err
        integer(kind=c_int) c_byte_size
        ! Private interface
        interface
            function c_multio_write_field_buffer(handle, metadata, data, byte_size) result(err) &
                bind(c, name='multio_write_field_buffer')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),         value, intent(in) :: handle
                type(c_ptr),         value, intent(in) :: metadata
                type(c_ptr),         value, intent(in) :: data
                integer(kind=c_int), value, intent(in) :: byte_size
                integer(c_int) :: err
            end function c_multio_write_field_buffer
        end interface
        ! Implementation
        c_byte_size = int(data%byte_size(),c_int)
        c_err = c_multio_write_field_buffer(handle%c_ptr(), metadata%c_ptr(), data%c_ptr(), c_byte_size)
        ! Setting return value
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_handle_write_field_buffer


    !> @brief Send a raw grib message
    !!
    !! This function sends a grib message to the multio of the object
    !! pointed to by the provided handle.
    !!
    !! @param [in,out] handle   A pointer to the object handle.
    !! @param [in]     message  GRIB message
    !!
    !! @return An error code indicating the operation's success.
    function multio_handle_write_grib_encoded(handle, message) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_double
        ! Variable references from the project
        use :: multio_api_metadata_mod,  only: multio_metadata
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_handle),                        intent(inout) :: handle
        character(len=1),         dimension(:), target, intent(in) :: message

        ! Function result
        integer :: err

#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: c_size
        ! Private interface to the c API
        interface
            function c_multio_write_grib_encoded(handle, gribdata, gribsize) result(err) &
                bind(c, name='multio_write_grib_encoded')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: handle
                type(c_ptr),    value, intent(in) :: gribdata
                integer(c_int), value, intent(in) :: gribsize
                integer(c_int) :: err
            end function c_multio_write_grib_encoded
        end interface
        ! Implementation
        c_size = int(size(message),c_int)
        c_err = c_multio_write_grib_encoded(handle%c_ptr(), c_loc(message), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_handle_write_grib_encoded


    !> @brief Get the field accepted flag.
    !!
    !! This function retrieves the field accepted flag from the object pointed to by the provided handle.
    !!
    !! @param [in,out] handle    A pointer to the object handle.
    !! @param [in]     metadata  Metadata to be sent with the field.
    !! @param [out]    set_value The flag value to be retrieved.
    !!
    !! @return An error code indicating the operation's success.
    !!
    function multio_handle_field_accepted(handle, metadata, set_value) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_bool
        ! Variable references from the project
        use :: multio_api_metadata_mod,  only: multio_metadata
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_handle),   intent(inout) :: handle
        class(multio_metadata), intent(inout) :: metadata
        logical,                intent(out)   :: set_value
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        logical(kind=c_bool) :: c_set_value
        ! Private interface to the c API
        interface
            function c_multio_field_accepted(handle, metadata, set_value) result(err) &
                bind(c, name='multio_field_accepted')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_bool
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in)  :: handle
                type(c_ptr), value, intent(in)  :: metadata
                logical(c_bool),    intent(out) :: set_value
                integer(c_int) :: err
            end function c_multio_field_accepted
        end interface
        ! Implementation
        c_err = c_multio_field_accepted(handle%c_ptr(), metadata%c_ptr(), c_set_value)
        ! Setting return values
        set_value = logical(c_set_value,kind(set_value))
        err = int(c_err,kind(err))
#else
        set_value = .true.
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_handle_field_accepted

end module multio_api_handle_mod
