!> @file
!!
!! @brief Definition of a class for encapsulating data passed to multio.
!!
!! This module provides a class that serves as a wrapper around eckit::Buffer,
!! facilitating the encapsulation of data intended for use within the multio system.
!!
!! @see eckit::Buffer
!!

module multio_api_data_mod

    use, intrinsic :: iso_c_binding, only: c_null_ptr
    use, intrinsic :: iso_c_binding, only: c_ptr
    use, intrinsic :: iso_c_binding, only: c_int

implicit none

    !> Default visibility of the module
    private

    !>
    !! @class datatype used to wrap eckit::buffer
    type :: multio_data

        !! Deafult visibility of the members
        private

        !! Pointer to the opaque c object
        type(c_ptr) :: impl = c_null_ptr

        !! Size of the datatype used
        integer(kind=c_int) :: byte_size_ = 0_c_int

    contains

        ! General management
        procedure :: new    => multio_data_new
        procedure :: delete => multio_data_delete
        procedure :: c_ptr  => multio_data_c_ptr

        ! Utils
        procedure :: resize    => multio_data_resize
        procedure :: size      => multio_data_size
        procedure :: zero      => multio_data_zero
        procedure :: byte_size => multio_data_byte_size

        ! Methods used to set values
        procedure :: set_float_scalar  => multio_data_set_float_scalar
        procedure :: set_double_scalar => multio_data_set_double_scalar
        procedure :: set_float_chunk   => multio_data_set_float_chunk
        procedure :: set_double_chunk  => multio_data_set_double_chunk
        generic   :: set => set_float_scalar, set_double_scalar, set_float_chunk, set_double_chunk

    end type ! multio_data

    !> Public symbols whitelist
    public :: multio_data

contains


    !> @brief Extract the C pointer of the data object.
    !!
    !! This subroutine extracts and returns the C pointer associated with the
    !! data object handled by the provided data context.
    !!
    !! @param[in,out] data - A handle passed as an object pointer.
    !!
    !! @return A C pointer to the data object.
    !!         If the object is not available, a NULL pointer is returned.
    !!
    function multio_data_c_ptr( data ) result(loc)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_ptr
        use, intrinsic :: iso_c_binding, only: c_null_ptr
    implicit none
        ! Dummy arguments
        class(multio_data), target, intent(inout) :: data
        ! Function result
        type(c_ptr) :: loc
#if !defined(MULTIO_DUMMY_API)
        ! Implementation
        loc = data%impl
#else
        loc = c_null_ptr
#endif
        ! Exit point
        return
    end function multio_data_c_ptr


    !> @brief Create a new data object.
    !!
    !! This subroutine creates a new data object with the specified byte size for the datatype used,
    !! and returns an error code indicating the operation's success.
    !!
    !! @param [in,out] data      - A handle passed as an object pointer.
    !! @param [in,out] handle    - The multio handle where the metadata is attached to.
    !! @param [in]     byte_size - The byte size of the datatype used.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_data_delete
    !!
    function multio_data_new(data, handle, byte_size) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding,   only: c_int
        ! Variable references from the project
        use :: multio_api_base_handle_mod, only: multio_base_handle
        use :: multio_api_constants_mod,   only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_data),        intent(inout) :: data
        class(multio_base_handle), intent(inout) :: handle
        integer,                   intent(in)    :: byte_size
        ! Function Result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_data_new(data, handle) result(err) &
                bind(c, name='multio_data_new')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),        intent(out) :: data
                type(c_ptr), value, intent(in)  :: handle
                integer(c_int) :: err
            end function c_multio_data_new
        end interface
        ! Call the c API
        data%byte_size_ = int(byte_size,c_int)
        c_err = c_multio_data_new(data%impl, handle%c_ptr())
        ! Output cast and cleanup
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_data_new


    !> @brief Delete a data object.
    !!
    !! This subroutine deletes the specified data object referenced by the provided handle
    !! and returns an error code indicating the operation's success.
    !!
    !! @param[in,out] data - A handle passed as an object pointer.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_data_new
    !!
    function multio_data_delete(data) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_null_ptr
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_data), intent(inout) :: data
        ! Function Result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_data_delete(data) result(err) &
                bind(c, name='multio_data_delete')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: data
                integer(c_int) :: err
            end function c_multio_data_delete
        end interface
        ! Call the c API
        c_err = c_multio_data_delete(data%c_ptr())
        data%impl = c_null_ptr
        data%byte_size_ = 0_c_int
        ! Output cast and cleanup
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_data_delete


    !> @brief Reset the memory of the object to zero.
    !!
    !! This subroutine resets the memory of the specified data object referenced by the provided handle
    !! to zero and returns an error code indicating the operation's success.
    !!
    !! @param[in,out] data - A handle passed as an object pointer.
    !!
    !! @return An error code indicating the operation's success.
    !!
    function multio_data_zero(data) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_data), intent(inout) :: data
        ! Function Result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_data_zero(data) result(err) &
                bind(c, name='multio_data_zero')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: data
                integer(c_int) :: err
            end function c_multio_data_zero
        end interface
        ! Call the c API
        c_err = c_multio_data_zero(data%c_ptr())
        ! Output cast and cleanup
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_data_zero


    !> @brief Resize a data object.
    !!
    !! This subroutine resizes the specified data object referenced by the provided handle
    !! to the new size indicated and returns an error code indicating the operation's success.
    !!
    !! @param[in,out] data - A handle passed as an object pointer.
    !! @param[in] new_size - The new size to be set for the object.
    !!
    !! @return An error code indicating the operation's success.
    !!
    function multio_data_resize(data,new_size) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_data), intent(inout) :: data
        integer,            intent(in)    :: new_size
        ! Function Result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_size
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_data_resize(data,new_size) result(err) &
                bind(c, name='multio_data_resize')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),         value, intent(in) :: data
                integer(kind=c_int), value, intent(in) :: new_size
                integer(c_int) :: err
            end function c_multio_data_resize
        end interface
        ! Call the c API
        c_size = int(new_size,c_int)
        c_err = c_multio_data_resize(data%c_ptr(), c_size)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_data_resize


    !> @brief Get the size of a data object.
    !!
    !! This function retrieves the size of a data object pointed to by the given handle.
    !!
    !! @param [in,out] data   A pointer to the object handle.
    !! @param [out]    size   The size of the object.
    !!
    !! @return An error code indicating the operation's success.
    !!
    function multio_data_size(data,size) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_data), intent(inout) :: data
        integer,intent(out) :: size
        ! Function Result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int), target :: c_size
        ! Private interface to the c API
        interface
            function c_multio_data_size(data,size) result(err) &
                bind(c, name='multio_data_size')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: data
                type(c_ptr), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_data_size
        end interface
        ! Call the c API
        c_err = c_multio_data_size(data%c_ptr(), c_loc(c_size))
        ! Output cast and cleanup
        size = int(c_size,kind(size))
        err = int(c_err,kind(err))
#else
        size = int(0,kind(size))
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_data_size


    !> @brief Get the byte size of the datatype used.
    !!
    !! This function retrieves the byte size of the datatype associated with the provided handle.
    !! For example, the value 4 corresponds to the datatype float, and 8 corresponds to double.
    !!
    !! @param [in] data   A pointer to the object handle.
    !!
    !! @return The byte size of the datatype.
    !!
    function multio_data_byte_size(data) result(byte_size)
    implicit none
        ! Dummy arguments
        class(multio_data), intent(in) :: data
        ! Function Result
        integer :: byte_size
#if !defined(MULTIO_DUMMY_API)
        ! Implementation
        byte_size = int(data%byte_size_,kind(byte_size))
#else
        byte_size = int(0,kind(byte_size))
#endif
        ! Exit point
        return
    end function multio_data_byte_size


    !> @brief Set a float element at a predefined position.
    !!
    !! This function sets a specified float value at a predefined position within the object
    !! pointed to by the provided handle.
    !!
    !! @param [in,out] data    A pointer to the object handle.
    !! @param [in]     value   The value to be set.
    !! @param [in]     pos     The position at which to set the value.
    !!
    !! @return An error code indicating the operation's success.
    !!
    function multio_data_set_float_scalar(data, value, pos) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_float
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_data),         intent(inout) :: data
        real(kind=c_float), target, intent(in)    :: value
        integer(c_int),             intent(in)    :: pos
        ! Function Result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_pos
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_data_set_float_scalar(data, value, pos) result(err) &
                bind(c, name='multio_data_set_float_scalar')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: data
                type(c_ptr),    value, intent(in) :: value
                integer(c_int), value, intent(in) :: pos
                integer(c_int) :: err
            end function c_multio_data_set_float_scalar
        end interface
        ! Call the c API
        c_pos = int(pos,kind(c_pos))
        c_err = c_multio_data_set_float_scalar(data%c_ptr(), c_loc(value), pos)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_data_set_float_scalar


    !> @brief Set a chunk of float elements at a predefined position.
    !!
    !! This function sets a specified float value across a chunk of elements at a predefined
    !! position within the object pointed to by the provided handle.
    !!
    !! @param [in,out] data   A pointer to the object handle.
    !! @param [in]     value  The value to be set.
    !! @param [in]     pos    The starting position where to set the value.
    !! @param [in]     size   The size of the chunk to be set.
    !!
    !! @return An error code indicating the operation's success.
    !!
    function multio_data_set_float_chunk(data, value, pos, size) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_float
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_data),                       intent(inout) :: data
        real(kind=c_float), dimension(:), target, intent(in)    :: value
        integer,                                  intent(in)    :: pos
        integer,                                  intent(in)    :: size
        ! Function Result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_pos
        integer(kind=c_int) :: c_size
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_data_set_float_chunk(data, value, pos, size) result(err) &
                bind(c, name='multio_data_set_float_chunk')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: data
                type(c_ptr),    value, intent(in) :: value
                integer(c_int), value, intent(in) :: pos
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_data_set_float_chunk
        end interface
        ! Call the c API
        c_pos = int(pos,kind(c_pos))
        c_size = int(size,kind(c_size))
        c_err = c_multio_data_set_float_chunk(data%c_ptr(), c_loc(value), c_pos, c_size)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_data_set_float_chunk


    !> @brief Set a double element at a predefined position.
    !!
    !! This function sets a specified double value at a predefined position within the object
    !! pointed to by the provided handle.
    !!
    !! @param [in,out] data   A pointer to the object handle.
    !! @param [in]     value  The value to be set.
    !! @param [in]     pos    The position where to set the value.
    !!
    !! @return An error code indicating the operation's success.
    !!
    function multio_data_set_double_scalar(data, value, pos) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_double
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_data),          intent(inout) :: data
        real(kind=c_double), target, intent(in)    :: value
        integer,                     intent(in)    :: pos
        ! Function Result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_pos
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_data_set_double_scalar(data, value, pos) result(err) &
                bind(c, name='multio_data_set_double_scalar')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: data
                type(c_ptr),    value, intent(in) :: value
                integer(c_int), value, intent(in) :: pos
                integer(c_int) :: err
            end function c_multio_data_set_double_scalar
        end interface
        ! Call the c API
        c_pos = int(pos,kind(c_pos))
        c_err = c_multio_data_set_double_scalar(data%c_ptr(), c_loc(value), c_pos)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_data_set_double_scalar


    !> @brief Set a chunk of double elements at a predefined position.
    !!
    !! This function sets a specified double value across a chunk of elements at a predefined
    !! position within the object pointed to by the provided handle.
    !!
    !! @param [in,out] data   A pointer to the object handle.
    !! @param [in]     value  The value to be set.
    !! @param [in]     pos    The starting position where to set the value.
    !! @param [in]     size   The size of the chunk to be set.
    !!
    !! @return An error code indicating the operation's success.
    !!
    function multio_data_set_double_chunk(data, value, pos, size) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_double
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_data),                        intent(inout) :: data
        real(kind=c_double), dimension(:), target, intent(in)    :: value
        integer,                                   intent(in)    :: pos
        integer,                                   intent(in)    :: size
        ! Function Result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_pos
        integer(kind=c_int) :: c_size
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_data_set_double_chunk(data, value, pos, size) result(err) &
                bind(c, name='multio_data_set_double_chunk')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: data
                type(c_ptr),    value, intent(in) :: value
                integer(c_int), value, intent(in) :: pos
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_data_set_double_chunk
        end interface
        ! Call the c API
        c_pos  = int(pos,kind(c_pos))
        c_size = int(size,kind(c_size))
        c_err  = c_multio_data_set_double_chunk(data%c_ptr(), c_loc(value), c_pos, c_size)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_data_set_double_chunk

end module multio_api_data_mod
