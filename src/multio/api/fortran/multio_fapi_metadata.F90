!> @file
!!
!! @brief Definition of a class used to wrap metadata to be passed to multio.
!!

module multio_api_metadata_mod

    use, intrinsic :: iso_c_binding, only: c_ptr
    use, intrinsic :: iso_c_binding, only: c_null_ptr

implicit none

    ! Default visibility of the module
    private

    !>
    !! @class datatype used to wrap the functionalities of a multio_metadata object
    type :: multio_metadata

        !! Deafult visibility of the members
        private

        !! Pointer to the opaque c object
        type(c_ptr) :: impl = c_null_ptr

    contains

        ! General management of the object
        procedure, public, pass :: new          => multio_new_metadata
        procedure, public, pass :: copy         => multio_copy_metadata
        procedure, public, pass :: delete       => multio_delete_metadata
        procedure, public, pass :: c_ptr        => multio_metadata_c_ptr


        ! Overload string
        procedure, public, pass :: set_string   => multio_metadata_set_string

        ! Overload integers
        procedure, private, pass :: set_int8     => multio_metadata_set_int8
        procedure, private, pass :: set_int16    => multio_metadata_set_int16
        procedure, private, pass :: set_int32    => multio_metadata_set_int32
        procedure, private, pass :: set_int64    => multio_metadata_set_int64
        generic,   public        :: set_int      => set_int8, &
                                                 &  set_int16, &
                                                 &  set_int32, &
                                                 &  set_int64

        ! Overload bool
        procedure, private, pass :: set_fbool    => multio_metadata_set_fbool
        procedure, private, pass :: set_cbool    => multio_metadata_set_cbool
        generic,   public        :: set_bool     => set_cbool, set_fbool

        ! Oversload real
        procedure, private, pass :: set_real32   => multio_metadata_set_real32
        procedure, private, pass :: set_real64   => multio_metadata_set_real64
        generic,   public        :: set_real     => set_real32, set_real64

        ! Overload all datatypes
        generic, public :: set => set_string, &
                               &  set_int8,   &
                               &  set_int16,  &
                               &  set_int32,  &
                               &  set_int64,  &
                               &  set_cbool,  &
                               &  set_fbool,  &
                               &  set_real32, &
                               &  set_real64
    end type ! multio_metadata

    ! Public symbols whitelist
    public :: multio_metadata

contains


!> @brief Extract the C pointer of the metadata object.
!!
!! This function extracts the C pointer of the metadata object from the provided handle.
!!
!! @param [in,out] metadata A pointer to the metadata object handle.
!!
!! @return The C pointer to the metadata object.
!!
    function multio_metadata_c_ptr( metadata ) result(loc)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_ptr
        use, intrinsic :: iso_c_binding, only: c_null_ptr
    implicit none
        ! Dummy arguments
        class(multio_metadata), target, intent(inout) :: metadata
        ! Function result
        type(c_ptr) :: loc
#if !defined(MULTIO_DUMMY_API)
        ! Implementation
        loc = metadata%impl
#else
        loc = c_null_ptr
#endif
        ! Exit point
        return
    end function multio_metadata_c_ptr


    !> @brief Create a new metadata object.
    !!
    !! This function creates a new metadata object and attaches it to the provided multio handle.
    !!
    !! @param [in,out] metadata A pointer to the metadata object handle.
    !! @param [in]     handle   The multio handle where the metadata is attached to.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_copy_metadata
    !! @see multio_delete_metadata
    !!
    function multio_new_metadata(metadata, handle) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding,   only: c_int
        ! Variable references from the project
        use :: multio_api_base_handle_mod, only: multio_base_handle
        use :: multio_api_constants_mod,   only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_metadata),    intent(inout) :: metadata
        class(multio_base_handle), intent(inout) :: handle
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_new_metadata(metadata, handle) result(err) &
                    bind(c, name='multio_new_metadata')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),        intent(out) :: metadata
                type(c_ptr), value, intent(in)  :: handle
                integer(c_int) :: err
            end function c_multio_new_metadata
        end interface
        ! Call the c API
        c_err = c_multio_new_metadata(metadata%impl, handle%c_ptr() )
        ! Output cast and cleanup
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_new_metadata


    !> @brief Create a new metadata object from an existing metadata object.
    !!
    !! This function creates a new metadata object from an existing metadata objects
    !! and attaches it to the provided multio handle.
    !!
    !! @param [in,out] metadata A pointer to the metadata object handle.
    !! @param [in]     mdFrom   The existing metadata object that is copied
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_new_metadata
    !! @see multio_delete_metadata
    !!
    function multio_copy_metadata(metadata, mdFrom) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding,   only: c_int
        ! Variable references from the project
        use :: multio_api_base_handle_mod, only: multio_base_handle
        use :: multio_api_constants_mod,   only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_metadata),    intent(inout) :: metadata
        class(multio_metadata),    intent(inout) :: mdFrom
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_copy_metadata(metadata, mdFrom) result(err) &
                    bind(c, name='multio_copy_metadata')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),        intent(out) :: metadata
                type(c_ptr), value, intent(in)  :: mdFrom
                integer(c_int) :: err
            end function c_multio_copy_metadata
        end interface
        ! Call the c API
        c_err = c_multio_copy_metadata(metadata%impl, mdFrom%impl )
        ! Output cast and cleanup
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_copy_metadata


    !> @brief Delete a metadata object.
    !!
    !! This function deletes a metadata object associated with the provided handle.
    !!
    !! @param [in,out] metadata A pointer to the metadata object handle.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_new_metadata
    !! @see multio_copy_metadata
    !!
    function multio_delete_metadata(metadata) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_metadata), intent(inout) :: metadata
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_delete_metadata(metadata) result(err) &
                    bind(c, name='multio_delete_metadata')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: metadata
                integer(c_int) :: err
            end function c_multio_delete_metadata
        end interface
        ! Call the c API
        c_err = c_multio_delete_metadata(metadata%c_ptr())
        ! Output cast and cleanup
        metadata%impl = c_null_ptr
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_delete_metadata


    !> @brief Set a new key-value pair with a string value.
    !!
    !! This function sets a new key-value pair with a string value in the metadata.
    !!
    !! @param [in,out] metadata A handle passed object pointer.
    !! @param [in]     key      The key to be set.
    !! @param [in]     value    The value to be set.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_metadata_set_string
    !! @see multio_metadata_set_int8
    !! @see multio_metadata_set_int16
    !! @see multio_metadata_set_int32
    !! @see multio_metadata_set_int64
    !! @see multio_metadata_set_real32
    !! @see multio_metadata_set_real64
    !! @see multio_metadata_set_fbool
    !! @see multio_metadata_set_cbool
    !!
    function multio_metadata_set_string(metadata, key, value) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_char
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_null_char
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_metadata), intent(inout) :: metadata
        character(len=*),       intent(in)    :: key
        character(len=*),       intent(in)    :: value
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        character(:,kind=c_char), allocatable, target :: nullified_key
        character(:,kind=c_char), allocatable, target :: nullified_value
        ! Private interface to the c API
        interface
            function c_multio_metadata_set_string(metadata, key, value) result(err) &
                    bind(c, name='multio_metadata_set_string')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: metadata
                type(c_ptr), value, intent(in) :: key
                type(c_ptr), value, intent(in) :: value
                integer(c_int) :: err
            end function c_multio_metadata_set_string
        end interface
        ! Initialization and allocation
        nullified_key = trim(key) // c_null_char
        nullified_value = trim(value) // c_null_char
        ! Call the c API
        c_err = c_multio_metadata_set_string(metadata%c_ptr(), c_loc(nullified_key), c_loc(nullified_value))
        ! Output cast and cleanup
        if (allocated(nullified_key)) deallocate(nullified_key)
        if (allocated(nullified_value)) deallocate(nullified_value)
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_metadata_set_string


    !> @brief Set a new key-value pair with an integer value (8 bits wide).
    !!
    !! This function sets a new key-value pair with an integer value in the metadata.
    !!
    !! @param [in,out] metadata A handle passed object pointer.
    !! @param [in]     key      The key to be set.
    !! @param [in]     value    The value to be set.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_metadata_set_string
    !! @see multio_metadata_set_int16
    !! @see multio_metadata_set_int32
    !! @see multio_metadata_set_int64
    !! @see multio_metadata_set_real32
    !! @see multio_metadata_set_real64
    !! @see multio_metadata_set_fbool
    !! @see multio_metadata_set_cbool
    !!
    function multio_metadata_set_int8(metadata, key, value) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding,   only: c_loc
        use, intrinsic :: iso_c_binding,   only: c_int
        use, intrinsic :: iso_c_binding,   only: c_char
        use, intrinsic :: iso_c_binding,   only: c_null_char
        use, intrinsic :: iso_fortran_env, only: int8
        ! Variable references from the project
        use :: multio_api_constants_mod,   only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_metadata), intent(inout) :: metadata
        character(len=*),       intent(in)    :: key
        integer(kind=int8),     intent(in)    :: value
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: c_value
        character(:,kind=c_char), allocatable, target :: nullified_key
        ! Private interface to the c API
        interface
            function c_multio_metadata_set_int(metadata, key, value) result(err) &
                    bind(c, name='multio_metadata_set_int')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: key
                integer(c_int), value, intent(in) :: value
                integer(c_int) :: err
            end function c_multio_metadata_set_int
        end interface
        ! Initialization and allocation
        nullified_key = trim(key) // c_null_char
        c_value = int(value,c_int)
        ! Call the c API
        c_err = c_multio_metadata_set_int(metadata%c_ptr(), c_loc(nullified_key), c_value)
        ! Output cast and cleanup
        if (allocated(nullified_key)) deallocate(nullified_key)
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_metadata_set_int8


    !> @brief Set a new key-value pair with an integer value (16 bits wide).
    !!
    !! This function sets a new key-value pair with an integer value in the metadata.
    !!
    !! @param [in,out] metadata A handle passed object pointer.
    !! @param [in]     key      The key to be set.
    !! @param [in]     value    The value to be set.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_metadata_set_string
    !! @see multio_metadata_set_int8
    !! @see multio_metadata_set_int32
    !! @see multio_metadata_set_int64
    !! @see multio_metadata_set_real32
    !! @see multio_metadata_set_real64
    !! @see multio_metadata_set_fbool
    !! @see multio_metadata_set_cbool
    !!
    function multio_metadata_set_int16(metadata, key, value) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding,   only: c_loc
        use, intrinsic :: iso_c_binding,   only: c_int
        use, intrinsic :: iso_c_binding,   only: c_char
        use, intrinsic :: iso_c_binding,   only: c_null_char
        use, intrinsic :: iso_fortran_env, only: int16
        ! Variable references from the project
        use :: multio_api_constants_mod,   only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_metadata), intent(inout) :: metadata
        character(len=*),       intent(in)    :: key
        integer(kind=int16),    intent(in)    :: value
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: c_value
        character(:,kind=c_char), allocatable, target :: nullified_key
        ! Private interface to the c API
        interface
            function c_multio_metadata_set_int(metadata, key, value) result(err) &
                    bind(c, name='multio_metadata_set_int')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: key
                integer(c_int), value, intent(in) :: value
                integer(c_int) :: err
            end function c_multio_metadata_set_int
        end interface
        ! Initialization and allocation
        nullified_key = trim(key) // c_null_char
        c_value = int(value,c_int)
        ! Call the c API
        c_err = c_multio_metadata_set_int(metadata%c_ptr(), c_loc(nullified_key), c_value)
        ! Output cast and cleanup
        if (allocated(nullified_key)) deallocate(nullified_key)
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_metadata_set_int16


    !> @brief Set a new key-value pair with an integer value (32 bits wide).
    !!
    !! This function sets a new key-value pair with an integer value in the metadata.
    !!
    !! @param [in,out] metadata A handle passed object pointer.
    !! @param [in]     key      The key to be set.
    !! @param [in]     value    The value to be set.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_metadata_set_string
    !! @see multio_metadata_set_int8
    !! @see multio_metadata_set_int16
    !! @see multio_metadata_set_int64
    !! @see multio_metadata_set_real32
    !! @see multio_metadata_set_real64
    !! @see multio_metadata_set_fbool
    !! @see multio_metadata_set_cbool
    !!
    function multio_metadata_set_int32(metadata, key, value) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding,   only: c_loc
        use, intrinsic :: iso_c_binding,   only: c_int
        use, intrinsic :: iso_c_binding,   only: c_char
        use, intrinsic :: iso_c_binding,   only: c_null_char
        use, intrinsic :: iso_fortran_env, only: int32
        ! Variable references from the project
        use :: multio_api_constants_mod,   only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_metadata), intent(inout) :: metadata
        character(len=*),       intent(in)    :: key
        integer(kind=int32),    intent(in)    :: value
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: c_value
        character(:,kind=c_char), allocatable, target :: nullified_key
        ! Private interface to the c API
        interface
            function c_multio_metadata_set_int(metadata, key, value) result(err) &
                    bind(c, name='multio_metadata_set_int')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: key
                integer(c_int), value, intent(in) :: value
                integer(c_int) :: err
            end function c_multio_metadata_set_int
        end interface
        ! Initialization and allocation
        nullified_key = trim(key) // c_null_char
        c_value = int(value,c_int)
        ! Call the c API
        c_err = c_multio_metadata_set_int(metadata%c_ptr(), c_loc(nullified_key), c_value)
        ! Output cast and cleanup
        if (allocated(nullified_key)) deallocate(nullified_key)
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_metadata_set_int32


    !> @brief Set a new key-value pair with an integer value (64 bits wide).
    !!
    !! This function sets a new key-value pair with an integer value in the metadata.
    !!
    !! @param [in,out] metadata A handle passed object pointer.
    !! @param [in]     key      The key to be set.
    !! @param [in]     value    The value to be set.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_metadata_set_string
    !! @see multio_metadata_set_int8
    !! @see multio_metadata_set_int16
    !! @see multio_metadata_set_int32
    !! @see multio_metadata_set_real32
    !! @see multio_metadata_set_real64
    !! @see multio_metadata_set_fbool
    !! @see multio_metadata_set_cbool
    !!
    function multio_metadata_set_int64(metadata, key, value) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding,   only: c_loc
        use, intrinsic :: iso_c_binding,   only: c_int
        use, intrinsic :: iso_c_binding,   only: c_long
        use, intrinsic :: iso_c_binding,   only: c_char
        use, intrinsic :: iso_c_binding,   only: c_null_char
        use, intrinsic :: iso_fortran_env, only: int64
        ! Variable references from the project
        use :: multio_api_constants_mod,   only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_metadata), intent(inout) :: metadata
        character(len=*),       intent(in)    :: key
        integer(kind=int64),    intent(in)    :: value
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_long) :: c_value
        character(:,kind=c_char), allocatable, target :: nullified_key
        ! Private interface to the c API
        interface
            function c_multio_metadata_set_long(metadata, key, value) result(err) &
                    bind(c, name='multio_metadata_set_long')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
                use, intrinsic :: iso_c_binding, only: c_long
            implicit none
                type(c_ptr),     value, intent(in) :: metadata
                type(c_ptr),     value, intent(in) :: key
                integer(c_long), value, intent(in) :: value
                integer(c_int) :: err
            end function c_multio_metadata_set_long
        end interface
        ! Initialization and allocation
        nullified_key = trim(key) // c_null_char
        c_value = int(value,c_int)
        ! Call the c API
        c_err = c_multio_metadata_set_long(metadata%c_ptr(), c_loc(nullified_key), c_value)
        ! Output cast and cleanup
        if (allocated(nullified_key)) deallocate(nullified_key)
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_metadata_set_int64


    !> @brief Set a new key-value pair with a float value.
    !!
    !! This function sets a new key-value pair with a float value in the metadata.
    !!
    !! @param [in,out] metadata A handle passed object pointer.
    !! @param [in]     key      The key to be set.
    !! @param [in]     value    The value to be set.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_metadata_set_string
    !! @see multio_metadata_set_int8
    !! @see multio_metadata_set_int16
    !! @see multio_metadata_set_int32
    !! @see multio_metadata_set_int64
    !! @see multio_metadata_set_real64
    !! @see multio_metadata_set_fbool
    !! @see multio_metadata_set_cbool
    !!
    function multio_metadata_set_real32(metadata, key, value) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding,   only: c_int
        use, intrinsic :: iso_c_binding,   only: c_int
        use, intrinsic :: iso_c_binding,   only: c_float
        use, intrinsic :: iso_c_binding,   only: c_char
        use, intrinsic :: iso_c_binding,   only: c_loc
        use, intrinsic :: iso_c_binding,   only: c_null_char
        use, intrinsic :: iso_fortran_env, only: real32
        ! Variable references from the project
        use :: multio_api_constants_mod,   only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_metadata), intent(inout) :: metadata
        character(len=*),       intent(in)    :: key
        real(kind=real32),      intent(in)    :: value
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        real(kind=c_float)  :: c_value
        character(:,kind=c_char), allocatable, target :: nullified_key
        ! Private interface to the c API
        interface
            function c_multio_metadata_set_float(metadata, key, value) result(err) &
                    bind(c, name='multio_metadata_set_float')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
                use, intrinsic :: iso_c_binding, only: c_float
            implicit none
                type(c_ptr),   value, intent(in) :: metadata
                type(c_ptr),   value, intent(in) :: key
                real(c_float), value, intent(in) :: value
                integer(c_int) :: err
            end function c_multio_metadata_set_float
        end interface
        ! Initialization and allocation
        nullified_key = trim(key) // c_null_char
        c_value = real(value,c_float)
        ! Call the c API
        c_err = c_multio_metadata_set_float(metadata%c_ptr(), c_loc(nullified_key), c_value)
        ! Output cast and cleanup
        if (allocated(nullified_key)) deallocate(nullified_key)
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_metadata_set_real32


    !> @brief Set a new key-value pair with a float value.
    !!
    !! This function sets a new key-value pair with a float value in the metadata.
    !!
    !! @param [in,out] metadata A handle passed object pointer.
    !! @param [in]     key      The key to be set.
    !! @param [in]     value    The value to be set.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_metadata_set_string
    !! @see multio_metadata_set_int8
    !! @see multio_metadata_set_int16
    !! @see multio_metadata_set_int32
    !! @see multio_metadata_set_int64
    !! @see multio_metadata_set_real32
    !! @see multio_metadata_set_fbool
    !! @see multio_metadata_set_cbool
    !!
    function multio_metadata_set_real64(metadata, key, value) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding,   only: c_int
        use, intrinsic :: iso_c_binding,   only: c_double
        use, intrinsic :: iso_c_binding,   only: c_char
        use, intrinsic :: iso_c_binding,   only: c_loc
        use, intrinsic :: iso_c_binding,   only: c_null_char
        use, intrinsic :: iso_fortran_env, only: real64
        ! Variable references from the project
        use :: multio_api_constants_mod,   only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_metadata), intent(inout) :: metadata
        character(len=*),       intent(in)    :: key
        real(kind=real64),      intent(in)    :: value
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        real(kind=c_double) :: c_value
        character(:,kind=c_char), allocatable, target :: nullified_key
        ! Private interface to the c API
        interface
            function c_multio_metadata_set_double(metadata, key, value) result(err) &
                    bind(c, name='multio_metadata_set_double')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
                use, intrinsic :: iso_c_binding, only: c_double
            implicit none
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: key
                real(c_double), value, intent(in) :: value
                integer(c_int) :: err
            end function c_multio_metadata_set_double
        end interface
        ! Initialization and allocation
        nullified_key = trim(key) // c_null_char
        c_value = real(value,c_double)
        ! Call the c API
        c_err = c_multio_metadata_set_double(metadata%c_ptr(), c_loc(nullified_key), c_value)
        ! Output cast and cleanup
        if (allocated(nullified_key)) deallocate(nullified_key)
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_metadata_set_real64


    !> @brief Set a new key-value pair with a boolean value (fortran bool type).
    !!
    !! This function sets a new key-value pair with a boolean value in the metadata.
    !!
    !! @param [in,out] metadata A handle passed object pointer.
    !! @param [in]     key      The key to be set.
    !! @param [in]     value    The value to be set.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_metadata_set_string
    !! @see multio_metadata_set_int8
    !! @see multio_metadata_set_int16
    !! @see multio_metadata_set_int32
    !! @see multio_metadata_set_int64
    !! @see multio_metadata_set_real32
    !! @see multio_metadata_set_real64
    !! @see multio_metadata_set_cbool
    !!
    function multio_metadata_set_fbool(metadata, key, value) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_bool
        use, intrinsic :: iso_c_binding, only: c_char
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_null_char
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_metadata), intent(inout) :: metadata
        character(len=*),       intent(in)    :: key
        logical,                intent(in)    :: value
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        logical(kind=c_bool) :: c_value
        integer(kind=c_int)  :: c_err
        character(:,kind=c_char), allocatable, target :: nullified_key
        ! Private interface to the c API
        interface
            function c_multio_metadata_set_bool(metadata, key, value) result(err) &
                    bind(c, name='multio_metadata_set_bool')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
                use, intrinsic :: iso_c_binding, only: c_bool
            implicit none
                type(c_ptr),     value, intent(in) :: metadata
                type(c_ptr),     value, intent(in) :: key
                logical(c_bool), value, intent(in) :: value
                integer(c_int) :: err
            end function c_multio_metadata_set_bool
        end interface
        ! Initialization and allocation
        c_value = logical(value,kind(value))
        nullified_key = trim(key) // c_null_char
        ! Call the c API
        c_err = c_multio_metadata_set_bool(metadata%c_ptr(), c_loc(nullified_key), c_value)
        ! Output cast and cleanup
        if (allocated(nullified_key)) deallocate(nullified_key)
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_metadata_set_fbool


    !> @brief Set a new key-value pair with a boolean value (c bool type).
    !!
    !! This function sets a new key-value pair with a boolean value in the metadata.
    !!
    !! @param [in,out] metadata A handle passed object pointer.
    !! @param [in]     key      The key to be set.
    !! @param [in]     value    The value to be set.
    !!
    !! @return An error code indicating the operation's success.
    !!
    !! @see multio_metadata_set_string
    !! @see multio_metadata_set_int8
    !! @see multio_metadata_set_int16
    !! @see multio_metadata_set_int32
    !! @see multio_metadata_set_int64
    !! @see multio_metadata_set_real32
    !! @see multio_metadata_set_real64
    !! @see multio_metadata_set_fbool
    !!
    function multio_metadata_set_cbool(metadata, key, value) result(err)
        ! Variable references from the fortran language standard modules
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_bool
        use, intrinsic :: iso_c_binding, only: c_char
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_null_char
        ! Variable references from the project
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        class(multio_metadata), intent(inout) :: metadata
        character(len=*),       intent(in)    :: key
        logical(kind=c_bool),   intent(in)    :: value
        ! Function result
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        ! Local variables
        integer(kind=c_int) :: c_err
        character(:,kind=c_char), allocatable, target :: nullified_key
        ! Private interface to the c API
        interface
            function c_multio_metadata_set_bool(metadata, key, value) result(err) &
                    bind(c, name='multio_metadata_set_bool')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
                use, intrinsic :: iso_c_binding, only: c_bool
            implicit none
                type(c_ptr),     value, intent(in) :: metadata
                type(c_ptr),     value, intent(in) :: key
                logical(c_bool), value, intent(in) :: value
                integer(c_int) :: err
            end function c_multio_metadata_set_bool
        end interface
        ! Initialization and allocation
        nullified_key = trim(key) // c_null_char
        ! Call the c API
        c_err = c_multio_metadata_set_bool(metadata%c_ptr(), c_loc(nullified_key), value)
        ! Output cast and cleanup
        if (allocated(nullified_key)) deallocate(nullified_key)
        err = int(c_err,kind(err))
#else
        err = int(MULTIO_SUCCESS,kind(err))
#endif
        ! Exit point
        return
    end function multio_metadata_set_cbool

end module multio_api_metadata_mod
