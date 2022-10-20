! (C) Copyright 1996-2012 ECMWF.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
! In applying this licence, ECMWF does not waive the privileges and immunities 
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.
!
program multio_replay_nemo_fapi
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env
    use multio_api
    use fckit_module
    use fckit_mpi_module
    use mpi_f08 ! for error codes
    implicit none 

    integer :: rank, client_count, server_count 

    integer :: global_size = 105704
    integer :: level = 1
    integer :: step = 24
    


    type(multio_handle) :: mio
    integer(8) :: mio_parent_comm = MPI_UNDEFINED

    character(len=3), dimension(4) :: nemo_parameters = ["sst", "ssu", "ssv", "ssw" ] 
    integer, dimension(4) :: grib_param_id = [262101, 212101, 212151, 212202 ] 
    character(len=6), dimension(4) :: grib_grid_type = ["T grid", "U grid", "V grid", "W grid" ] 
    character(len=12), dimension(4) :: grib_level_type = ["oceanSurface", "oceanSurface", "oceanSurface", "oceanSurface" ] 

    interface
      function strlen(s) result(l) bind(c, name='strlen')
        use, intrinsic    :: iso_c_binding
        implicit none 

        character(c_char) :: s
        integer(c_int)    :: l
      end function
    end interface


    write(0,*) "Start programm multio_replay_nemo_fapi..."

    call init(mio, rank, server_count, client_count)
    call run(mio, rank, client_count, nemo_parameters, grib_param_id, grib_grid_type, grib_level_type, &
               global_size, level, step)

    call test_data(rank, nemo_parameters, grib_param_id, grib_grid_type, grib_level_type, &
               global_size, level, step)
contains

subroutine multio_custom_error_handler(context, err)
    integer(8), intent(inout) :: context  ! Use mpi communicator as context
    integer, intent(in) :: err
    type(fckit_mpi_comm) :: comm
    
    if (err /= MULTIO_SUCCESS) then
        write (error_unit, *) 'MULTIO ERROR: ',multio_error_string(err)
        write (error_unit, *) 'Abort mpi...'
        
        if (context /= MPI_UNDEFINED) then
            comm = fckit_mpi_comm(int(context))
            call comm%abort(MPI_ERR_OTHER)
            context = MPI_UNDEFINED
        endif
    endif
end subroutine



subroutine init(mio, rank, server_count, client_count)
    integer(kind=c_int) :: cerr
    integer :: ierror
    integer, intent(out) :: rank
    integer, intent(out) :: server_count
    integer, intent(out) :: client_count
    ! integer :: color_client
    integer :: key
    type(fckit_mpi_comm) :: comm
    type(fckit_mpi_comm) :: newcomm
    integer(c_int) :: newcomm_id
    type(multio_handle), intent(inout) :: mio
    type(multio_configurationcontext) :: cc
    ! for tests
    logical(c_bool) :: is_active


    write(0,*) "Init..."
    cerr = cc%new()
    if (cerr /= MULTIO_SUCCESS) ERROR STOP "Error creating default configuration context"
    
    cerr = cc%mpi_allow_world_default_comm(.FALSE._1)
    if (cerr /= MULTIO_SUCCESS) ERROR STOP "Error setting default multio mpi allow_world_default_comm"

    ! color_client = 777

    write(0,*) "multio_initialise..."
    cerr = multio_initialise()
    if (cerr /= MULTIO_SUCCESS) ERROR STOP 1

    write(0,*) "fckit_main init..."
    call fckit_main%init() ! Used for client comm

    comm = fckit_mpi_comm()
    rank = comm%rank()

    write(0,*) "add mpi comm nemo"
    call fckit_mpi_addComm("nemo", comm%communicator())
    ! newcomm = comm%split(color_client, "oce") ! Client splitting done by multio

    write(0,*) "multio_new..."
    newcomm_id = 0
    write(0,*) "set client id..."
    cerr = cc%mpi_client_id("oce")
    if (cerr /= MULTIO_SUCCESS) ERROR STOP "Error setting mpi client id to configuration context"
    write(0,*) "set parent comm..."
    mio_parent_comm = comm%communicator()
    cerr = cc%mpi_parent_comm(int(mio_parent_comm))
    if (cerr /= MULTIO_SUCCESS) ERROR STOP "Error setting mpi parent comm to configuration context"
    write(0,*) "set return client comm..."
    ! write (*,*) "newcomm_id ptr",loc(newcomm_id)
    cerr = cc%mpi_return_client_comm(newcomm_id)
    if (cerr /= MULTIO_SUCCESS) ERROR STOP "Error setting mpi client return comm to configuration context"
    write(0,*) "create new handle..."
    cerr = mio%new(cc) 
    if (cerr /= MULTIO_SUCCESS) ERROR STOP "Error creating new mpi handle"
    if (newcomm_id == 0) ERROR STOP "Return communicator has not been set as expected"
    newcomm = fckit_mpi_comm(newcomm_id)
    
    cerr = cc%delete() 
    if (cerr /= MULTIO_SUCCESS) ERROR STOP "Error deleting configuration context"

    server_count = comm%size()
    client_count = newcomm%size()
    server_count = server_count - client_count

    write(0,*) "client_count", client_count
    write(0,*) "server_count", server_count
    
    cerr = multio_set_failure_handler(multio_custom_error_handler, mio_parent_comm)
    if (cerr /= MULTIO_SUCCESS) then
         write(error_unit, *) 'setting multio failure handler failed: ',multio_error_string(cerr)
         ERROR STOP "MULTIO_ERROR"
    end if


    
    ! Performing a few tests
    cerr = mio%field_is_active("sst", is_active)
    if (.not. is_active) then
        ERROR STOP 'Field "sst" should be active'
    end if
    
    cerr = mio%field_is_active("ssv", is_active)
    if (.not. is_active) then
        ERROR STOP 'Field "ssv" should be active'
    end if
    
    cerr = mio%field_is_active("ssu", is_active)
    if (.not. is_active) then
        ERROR STOP 'Field "ssu" should be active'
    end if
    
    cerr = mio%field_is_active("ssw", is_active)
    if (.not. is_active) then
        ERROR STOP 'Field "ssw" should be active'
    end if
    
    cerr = mio%category_is_fully_active("ocean-domain-map", is_active)
    if (.not. is_active) then
        ERROR STOP 'Category "ocean-domain-map" should be completly active'
    end if
    
    cerr = mio%category_is_fully_active("ocean-mask", is_active)
    if (.not. is_active) then
        ERROR STOP 'Category "ocean-mask" should be fully active'
    end if
    
    cerr = mio%category_is_fully_active("ocean-2d", is_active)
    if (is_active) then
        ERROR STOP 'Category "ocean-2d" should not be fully active'
    end if
    
    cerr = mio%category_is_fully_active("ocean-3d", is_active)
    if (is_active) then
        ERROR STOP 'Category "ocean-3d" should not be fully active'
    end if
    
    cerr = mio%field_is_active("notexisting", is_active)
    if (is_active) then
        ERROR STOP 'Field "notexisting" should not be active'
    end if

end subroutine init


subroutine run(mio, rank, client_count, & 
        nemo_parameters, grib_param_id, grib_grid_type, grib_level_type, &
        global_size, level, step &
)
    integer(kind=c_int) :: cerr
    type(multio_handle), intent(inout) :: mio
    integer, intent(in) :: rank
    integer, intent(in) :: client_count
    integer, intent(in) :: global_size
    integer, intent(in) :: level
    integer, intent(in) :: step
    character(*), dimension(2), intent(in) :: nemo_parameters 
    integer, dimension(2), intent(in) :: grib_param_id 
    character(*), dimension(2), intent(in) :: grib_grid_type 
    character(*), dimension(2), intent(in) :: grib_level_type 


    write(0,*) "Run..."

    cerr = mio%open_connections()
    if (cerr /= MULTIO_SUCCESS) ERROR STOP 8

    call set_domains(mio, rank, client_count)

    call write_fields(mio, rank, client_count, nemo_parameters, &
      grib_param_id, grib_grid_type, grib_level_type, global_size, level, step)

    cerr = mio%close_connections()
    if (cerr /= MULTIO_SUCCESS) ERROR STOP 39
end subroutine run


subroutine read_grid(grid_type, client_id, domain_dims)
    integer(kind=c_int) :: cerr
    integer, intent(in) :: client_id
    character(len=*), intent(in) :: grid_type
    character(len(grid_type)+3) :: fname
    character(2) :: client_id_str
    character(len=128) :: grid_type_read
    integer :: FID = 1

    integer, intent(out) :: domain_dims(11)

    write(0,*) "read_grid", grid_type, client_id

    write(client_id_str,'(i0.2)') client_id
    fname=grid_type//'_'//client_id_str

    open(FID, file=fname, status='old', action='read')
    read(FID, *) grid_type_read, domain_dims
    IF( grid_type_read /= grid_type) THEN
        print *,"Wrong grid is beeding read: ", fname
        ERROR STOP 18
    END IF
    close(FID)
end subroutine read_grid


subroutine set_domains(mio, rank, client_count)
    integer(kind=c_int) :: cerr
    type(multio_handle), intent(inout) :: mio
    integer, intent(in) :: rank
    integer, intent(in) :: client_count
    type(multio_metadata) :: md
    character(len=6), dimension(4) :: grib_grid_type = ["T grid", "U grid", "V grid", "W grid" ] 
    character(len=6), dimension(4) :: grib_grid_fname = ["grid_T", "grid_U", "grid_V", "grid_W" ] 
    integer, dimension(11) :: buffer
    integer :: i

    write(0,*) "set_domains..."

    cerr = md%new()
    if (cerr /= MULTIO_SUCCESS) ERROR STOP 9

    do i=1, size(grib_grid_type)
        print *,i, grib_grid_type(i)
        call read_grid(grib_grid_fname(i), rank, buffer)

        cerr = md%set_string_value("name", grib_grid_type(i))
        if (cerr /= MULTIO_SUCCESS) ERROR STOP 10
        cerr = md%set_string_value("category", "ocean-domain-map")
        if (cerr /= MULTIO_SUCCESS) ERROR STOP 11
        cerr = md%set_string_value("representation", "structured")
        if (cerr /= MULTIO_SUCCESS) ERROR STOP 12
        cerr = md%set_bool_value("toAllServers", .TRUE._1)
        if (cerr /= MULTIO_SUCCESS) ERROR STOP 14

        cerr = mio%write_domain(md, buffer, size(buffer))
        if (cerr /= MULTIO_SUCCESS) ERROR STOP 15
    end do

    cerr = md%delete()
    if (cerr /= MULTIO_SUCCESS) ERROR STOP 17
end subroutine set_domains


subroutine write_fields(mio, rank, client_count, nemo_parameters, grib_param_id, grib_grid_type, grib_level_type, &
    global_size, level, step)
    integer(kind=c_int) :: cerr
    type(multio_handle), intent(inout) :: mio
    integer, intent(in) :: rank
    integer, intent(in) :: client_count
    type(multio_metadata) :: md
    integer, dimension(11) :: buffer
    character(*), dimension(2), intent(in) :: nemo_parameters 
    integer, dimension(2), intent(in) :: grib_param_id 
    character(*), dimension(2), intent(in) :: grib_grid_type 
    character(*), dimension(2), intent(in) :: grib_level_type 
    integer, intent(in):: global_size
    integer, intent(in):: level
    integer, intent(in):: step
    integer :: i

    real(kind=c_double), dimension(:), allocatable :: values


    write(0,*) "write_fields", rank, client_count

    cerr = md%new()
    if (cerr /= MULTIO_SUCCESS) ERROR STOP 19

    cerr = md%set_string_value("category", "ocean-2d")
    if (cerr /= MULTIO_SUCCESS) ERROR STOP 20
    cerr = md%set_int_value("globalSize", global_size)
    if (cerr /= MULTIO_SUCCESS) ERROR STOP 21
    cerr = md%set_int_value("level", level)
    if (cerr /= MULTIO_SUCCESS) ERROR STOP 22
    cerr = md%set_int_value("step", step)
    if (cerr /= MULTIO_SUCCESS) ERROR STOP 23

    cerr = md%set_double_value("missingValue", 0.0_8)
    if (cerr /= MULTIO_SUCCESS) ERROR STOP 31
    cerr = md%set_bool_value("bitmapPresent", .FALSE._1)
    if (cerr /= MULTIO_SUCCESS) ERROR STOP 32
    cerr = md%set_int_value("bitsPerValue", 16)
    if (cerr /= MULTIO_SUCCESS) ERROR STOP 33

    cerr = md%set_bool_value("toAllServers", .FALSE._1)
    if (cerr /= MULTIO_SUCCESS) ERROR STOP 34

    do i=1, size(nemo_parameters)
        print *,i, nemo_parameters(i)
        call read_field(nemo_parameters(i), rank, step, values)

        cerr = md%set_string_value("name", nemo_parameters(i))
        if (cerr /= MULTIO_SUCCESS) ERROR STOP 24
        cerr = md%set_string_value("nemoParam", nemo_parameters(i))
        if (cerr /= MULTIO_SUCCESS) ERROR STOP 25
        cerr = md%set_int_value("param", grib_param_id(i))
        if (cerr /= MULTIO_SUCCESS) ERROR STOP 26
        cerr = md%set_string_value("gridSubType", grib_grid_type(i))
        if (cerr /= MULTIO_SUCCESS) ERROR STOP 27
        cerr = md%set_string_value("domain", grib_grid_type(i))
        if (cerr /= MULTIO_SUCCESS) ERROR STOP 29
        cerr = md%set_string_value("typeOfLevel", grib_level_type(i))
        if (cerr /= MULTIO_SUCCESS) ERROR STOP 30

        cerr = mio%write_field(md, values, size(values))
        if (cerr /= MULTIO_SUCCESS) ERROR STOP 35

        deallocate(values)
    end do

    cerr = md%delete()
    if (cerr /= MULTIO_SUCCESS) ERROR STOP 36
end subroutine write_fields


subroutine read_field(param, client_id, step, values)
     integer(kind=c_int) :: cerr
     integer, intent(in) :: client_id
     integer, intent(in) :: step
     character(len=*), intent(in) :: param
     character(len(param)+6) :: fname
     character(2) :: client_id_str
     character(2) :: step_str
     integer :: FID = 2
     integer :: ioerror
     character(len=128) :: ioerrmsg
     integer :: number_doubles

     real(kind=c_double) :: dummy_double
     real(kind=c_double), dimension(:), allocatable, intent(out):: values


     write(0,*) "read_field ", param, client_id, step

     write(step_str,'(i0.2)') step
     write(client_id_str,'(i0.2)') client_id
     fname=param//'_'//step_str//'_'//client_id_str

     write(0,*) "read_field file ", fname, " recl: ", C_SIZEOF(dummy_double)

     number_doubles =0 
     open(FID, file=fname, status='old', action='read', access="stream", form="unformatted")
     loop1: DO
        read(FID, iostat=ioerror, iomsg=ioerrmsg) dummy_double
        ! write(0,*) "read_field loop ", number_doubles
        IF (ioerror < 0) THEN
         WRITE(*,*) trim(fname), ' :number of doubles =', number_doubles
         EXIT loop1
        ELSE IF (ioerror > 0) THEN
            write(0,*) "IO-Error ", ioerror, ": ", ioerrmsg
            ERROR STOP 'IO-error'
        ENDIF

        number_doubles = number_doubles + 1
     END DO loop1

     allocate(values(number_doubles))
     read(FID, pos=1) values
     close(FID)
end subroutine read_field


subroutine test_data(rank, & 
        nemo_parameters, grib_param_id, grib_grid_type, grib_level_type, &
        global_size, level, step &
)
    integer(kind=c_int) :: cerr
    integer, intent(in) :: rank
    integer, intent(in) :: global_size
    integer, intent(in) :: level
    integer, intent(in) :: step
    character(*), dimension(2), intent(in) :: nemo_parameters 
    integer, dimension(2), intent(in)      :: grib_param_id 
    character(*), dimension(2), intent(in) :: grib_grid_type 
    character(*), dimension(2), intent(in) :: grib_level_type 

    type(fckit_mpi_comm) :: comm

    character(len=128) :: fname
    character(len=128) :: refname
    character(len=128) :: param_id_str
    character(2) :: step_str
    character(2) :: level_str
    integer :: i
    integer :: FID1 = 3
    integer :: FID2 = 4
    integer :: byte_num
    integer(kind=1) :: actual
    integer(kind=1) :: expected

    integer :: ioerror
    character(len=128) :: ioerrmsg


    write(0,*) "Test data..."
    comm = fckit_mpi_comm()
    call comm%barrier()

    if (rank /= 0) return

    write(step_str,'(i0)') step
    write(level_str,'(i0)') level

    do i=1, size(nemo_parameters)
       write(param_id_str,'(i0)') grib_param_id(i)
       fname = trim(level_str)//'::'//trim(param_id_str)//'::'//trim(step_str)
       refname = nemo_parameters(i)//'_'//step_str//'_reference'

       write(0,*) "comparing file ", trim(fname), " with reference ", trim(refname)

       open(FID1, file=fname, status='old', action='read', access="stream", form="unformatted")
       open(FID2, file=fname, status='old', action='read', access="stream", form="unformatted")
       byte_num = 0
       loop1: DO
          read(FID1, iostat=ioerror, iomsg=ioerrmsg) actual
          IF (ioerror < 0) THEN
           WRITE(*,*) trim(fname), ' EOF of actual file: ', byte_num
           EXIT loop1
          ELSE IF (ioerror > 0) THEN
              write(0,*) "IO-Error File:", trim(fname), " Error:", ioerror, ": ", ioerrmsg
              ERROR STOP 'IO-error1'
          ENDIF

          read(FID2, iostat=ioerror, iomsg=ioerrmsg) expected
          IF (ioerror < 0) THEN
           WRITE(*,*) trim(refname), ' EOF of reference file: ', byte_num
           EXIT loop1
          ELSE IF (ioerror > 0) THEN
              write(0,*) "IO-Error File:", trim(refname), " Error:", ioerror, ": ", ioerrmsg
              ERROR STOP 'IO-error1'
          ENDIF

          IF (actual /= expected) THEN
               WRITE(*,*) "Error in bytewise comparison of files ", trim(fname), " and ", trim(refname), &
                    " at pos: ", byte_num, "Actual: ", actual, "Expected: ", expected
              ERROR STOP 'Differences in testdata'
          ENDIF

          byte_num = byte_num + 1
       END DO loop1

       close(FID1)
       close(FID2)
    end do
end subroutine test_data


end program multio_replay_nemo_fapi
