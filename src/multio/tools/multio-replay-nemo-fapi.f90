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
  use multio_api
  ! use mpi
  ! use mpi_f08
  use fckit_module
  implicit none 

  integer :: rank, client_count, server_count 

  integer :: global_size = 105704
  integer :: level = 1
  integer :: step = 24

  class(multio_handle), intent(inout) :: mio

  character(len=3), dimension(4) :: nemo_parameters = ["sst", "ssu", "ssv", "ssw" ] 
  integer, dimension(4) :: grib_param_id = [262101, 212101, 212151, 212202 ] 
  character(len=6), dimension(4) :: grib_grid_type = ["T grid", "U grid", "V grid", "W grid" ] 
  character(len=12), dimension(4) :: grib_level_type = ["oceanSurface", "oceanSurface", "oceanSurface", "oceanSurface" ] 

  interface
    function strlen(s) result(l) bind(c, name='strlen')
      use, intrinsic    :: iso_c_binding
      character(c_char) :: s
      integer(c_int)    :: l
    end function
  end interface

  write(0,*) "Init..."

  call init(mio, rank, server_count, client_count)
  call run(mio, rank, client_count, nemo_parameters, grib_param_id, grib_grid_type, grib_level_type, &
             global_size, level, step)
contains


subroutine init(mio, rank, server_count, client_count)
 implicit none
 integer(kind=C_INT) :: cerr
 integer :: ierror
 integer, INTENT(OUT) :: rank
 integer, INTENT(OUT) :: server_count
 integer, INTENT(OUT) :: client_count
 integer :: color_client, key
 ! type(MPI_COMM) :: newcomm
 type(fckit_mpi_comm) :: comm
 type(fckit_mpi_comm) :: newcomm
 class(multio_handle), intent(inout) :: mio


 color_client = 777

 cerr = multio_initialise()
 if (cerr /= MULTIO_SUCCESS) STOP 1

 cerr = mio%new_handle()
 if (cerr /= MULTIO_SUCCESS) STOP 2

 ! call MPI_INIT(ierror) ! done in multio
 call fckit_main%init() ! Used for client comm
 ! if (ierror /= 0) STOP 3
 !comm = fckit_mpi_comm()
 !call MPI_COMM_RANK(MPI_COMM_WORLD, rank_, ierror)
 rank_ = comm%rank()
 !if (ierror /= 0) STOP 4

 !call MPI_COMM_SPLIT(MPI_COMM_WORLD, color_client, rank, newcomm, ierror)
 newcomm = comm%split(color_client)
 !if (ierror /= 0) STOP 5

 !call MPI_COMM_SIZE(MPI_COMM_WORLD, server_count, ierror)
 server_count = comm%size()
 !if (ierror /= 0) STOP 6

 !call MPI_COMM_SIZE(newcomm, client_count, ierror)
 client_count = newcomm%size()
 !if (ierror /= 0) STOP 7

 server_count = server_count - client_count

 write(0,*) "client_count", client_count
 write(0,*) "server_count", server_count
end subroutine init

subroutine run(mio, rank, client_count, & 
        nemo_parameters, grib_param_id, grib_grid_type, grib_level_type, &
        global_size, level, step &
)
 implicit none
 integer(kind=C_INT) :: cerr
 class(multio_handle), intent(inout) :: mio
 integer, INTENT(IN) :: rank
 integer, INTENT(IN) :: client_count
 integer, INTENT(IN) :: global_size
 integer, INTENT(IN) :: level
 integer, INTENT(IN) :: step
 character(*), dimension(2), intent(in) :: nemo_parameters 
 integer, dimension(2), intent(in) :: grib_param_id 
 character(*), dimension(2), intent(in) :: grib_grid_type 
 character(*), dimension(2), intent(in) :: grib_level_type 

 cerr = mio%open_connections()
 if (cerr /= MULTIO_SUCCESS) STOP 8

 call set_domains(mio, rank, client_count)

 call write_fields(mio, rank, client_count, nemo_parameters, &
    grib_param_id, grib_grid_type, grib_level_type, global_size, level, step)

 cerr = mio%close_connections()
 if (cerr /= MULTIO_SUCCESS) STOP 39
end subroutine run

subroutine set_domains(mio, rank, client_count)
 implicit none
 integer(kind=C_INT) :: cerr
 class(multio_handle), intent(inout) :: mio
 integer, INTENT(IN) :: rank
 integer, INTENT(IN) :: client_count
 class(multio_metadata) :: md
 character(len=6), dimension(2) :: grib_grid_type = [charater(len=6) :: "T grid", "U grid", "V grid", "W grid" ] 
 character(len=6), dimension(2) :: grib_grid_fname = [charater(len=6) :: "grid_T", "grid_U", "grid_V", "grid_W" ] 
 integer, dimension(11) :: buffer

 cerr = md%new_metadata()
 if (cerr /= MULTIO_SUCCESS) STOP 9

 do i=1, size(grib_grid_type)
   print *,i, items(i)
   read_grid(grib_grid_fname(i), rank, buffer)

   cerr = md%set_string_value("name", grib_grid_type(i))
   if (cerr /= MULTIO_SUCCESS) STOP 10
   cerr = md%set_string_value("category", "ocean-domain-map")
   if (cerr /= MULTIO_SUCCESS) STOP 11
   cerr = md%set_string_value("representation", "structured")
   if (cerr /= MULTIO_SUCCESS) STOP 12
   cerr = md%set_int_value("domainCount", client_count)
   if (cerr /= MULTIO_SUCCESS) STOP 13
   cerr = md%set_bool_value("toAllServers", .TRUE.)
   if (cerr /= MULTIO_SUCCESS) STOP 14

   cerr = mio%write_domain(md, c_loc(buffer), 11)
   if (cerr /= MULTIO_SUCCESS) STOP 15
   cerr = md%reset_metadata
   if (cerr /= MULTIO_SUCCESS) STOP 16
 end do

 cerr = md%delete_metadata()
 if (cerr /= MULTIO_SUCCESS) STOP 17
end subroutine set_domains


subroutine read_grid(grid_type, client_id, domain_dims)
 implicit none
 integer(kind=C_INT) :: cerr
 integer, INTENT(IN) :: client_id
 character(len=*), intent(in) :: grid_type
 ! character(len=:), allocatable :: fname
 ! allocate( character(len=3+len(grid_type)) :: s )
 character(len(grid_type)+3) :: fname
 character(2) :: client_id_str
 character(6) :: grid_type_read
 integer :: FID = 1

 integer, dimension(11), intent(out) :: domain_dims


 write(client_id_str,'(i0)') client_id
 fname=grid_type//'_'//client_id_str

 open(FID, file=fname, status='old', action='read')
 read(FID, *) grid_type_read
 if( grid_type_read /= grid_type) THEN
   print *,"Wrong grid is beeding read: ", fname
   STOP 18
 END

 read(FID, '11i0') domain_dims
end subroutine read_grid

subroutine write_fields(mio, rank, client_count, nemo_parameters, grib_param_id, grib_grid_type, grib_level_type &
    global_size, level, step)
 implicit none
 integer(kind=C_INT) :: cerr
 class(multio_handle), intent(inout) :: mio
 integer, INTENT(IN) :: rank
 integer, INTENT(IN) :: client_count
 class(multio_metadata) :: md
 integer, dimension(11) :: buffer
 character(*), dimension(2), intent(in) :: nemo_parameters 
 integer, dimension(2), intent(in) :: grib_param_id 
 character(*), dimension(2), intent(in) :: grib_grid_type 
 character(*), dimension(2), intent(in) :: grib_level_type 
 integer, INTENT(IN):: global_size
 integer, INTENT(IN):: level
 integer, INTENT(IN):: step

 real(kind=C_DOUBLE), dimension(:), allocatable :: values

 cerr = md%new_metadata()
 if (cerr /= MULTIO_SUCCESS) STOP 19

 do i=1, size(nemo_parameters)
   print *,i, nemo_parameters(i)
   read_field(nemo_parameters(i), rank, step, values)

   cerr = md%set_string_value("category", "ocean-2d")
   if (cerr /= MULTIO_SUCCESS) STOP 20
   cerr = md%set_int_value("globalSize", global_size)
   if (cerr /= MULTIO_SUCCESS) STOP 21
   cerr = md%set_int_value("level", level_)
   if (cerr /= MULTIO_SUCCESS) STOP 22
   cerr = md%set_int_value("step", step_)
   if (cerr /= MULTIO_SUCCESS) STOP 23

   cerr = md%set_string_value("name", nemo_parameters(i))
   if (cerr /= MULTIO_SUCCESS) STOP 24
   cerr = md%set_string_value("nemoParam", nemo_parameters(i))
   if (cerr /= MULTIO_SUCCESS) STOP 25
   cerr = md%set_int_value("param", grib_param_id(i))
   if (cerr /= MULTIO_SUCCESS) STOP 26
   cerr = md%set_string_value("gridSubType", grib_grid_type(i))
   if (cerr /= MULTIO_SUCCESS) STOP 27
   cerr = md%set_int_value("domainCount", client_count)
   if (cerr /= MULTIO_SUCCESS) STOP 28
   cerr = md%set_string_value("domain", grib_grid_type(i))
   if (cerr /= MULTIO_SUCCESS) STOP 29
   cerr = md%set_string_value("typeOfLevel", grib_level_type(i))
   if (cerr /= MULTIO_SUCCESS) STOP 30

   cerr = md%set_double_value("missingValue", 0.0)
   if (cerr /= MULTIO_SUCCESS) STOP 31
   cerr = md%set_bool_value("bitmapPresent", .FALSE.)
   if (cerr /= MULTIO_SUCCESS) STOP 32
   cerr = md%set_int_value("bitsPerValue", 16)
   if (cerr /= MULTIO_SUCCESS) STOP 33

   cerr = md%set_bool_value("toAllServers", .FALSE.)
   if (cerr /= MULTIO_SUCCESS) STOP 34

   cerr = mio%write_field(md, values, size(values))
   if (cerr /= MULTIO_SUCCESS) STOP 35
   cerr = md%reset_metadata()
   if (cerr /= MULTIO_SUCCESS) STOP 35
 end do
 cerr = md%delete_metadata()
 if (cerr /= MULTIO_SUCCESS) STOP 36
end subroutine write_fields

subroutine read_field(param, client_id, step, values)
 implicit none
 integer(kind=C_INT) :: cerr
 integer, INTENT(IN) :: client_id
 integer, INTENT(IN) :: step
 character(len=*), intent(in) :: param
 character(len(param)+6) :: fname
 character(2) :: client_id_str
 character(2) :: step_str
 integer :: FID = 2
 integer :: ferror
 integer :: number_doubles = 0

 real(kind=C_DOUBLE) :: dummy_double
 real(kind=C_DOUBLE), dimension(:), allocatable, INTENT(OUT):: values

 write(step_str,'(i0)') step
 write(client_id_str,'(i0)') client_id
 fname=param//'_'//step_str//'_'//client_id_str

 loop1: DO
    read(FID, *, iostat=ferror) dummy_double
    IF (ferror < 0) THEN
     number_doubles = number_doubles + 1
     WRITE(*,*) trim(fname), ' :number of byes =', number_doubles-1
     EXIT loop1
    ELSE IF (ferror > 0) THEN
        STOP 'IO-error'
    ENDIF
        number_doubles = number_doubles + 1
 END DO loop1

 allocate(values(number_doubles))
 open(FID, file=fname, status='old', action='read', access="stream", form="unformatted")
 read(FID, *) values
end subroutine read_field

subroutine example_fortran_api_append
 implicit none
 type(C_PTR)                                   :: odb_handle, odb_it
 integer(kind=C_INT)                           :: cerr
 character(kind=C_CHAR, len=max_varlen)        :: config = C_NULL_CHAR
 character(kind=C_CHAR, len=max_varlen)        :: outputfile="example_fortran_api_append.odb"//achar(0)
 integer(kind=4)                               :: i
 integer(kind=C_INT)                           :: itype, c_ncolumns 
 real(kind=C_DOUBLE), dimension(:), allocatable:: one_row
 integer(kind=c_int)                           :: offsets(ncolumns)
 integer(kind=c_int)                           :: row_length_doubles
 character(len=100)                            :: expver="fihn"//achar(0)
 character(len=100)                            :: wigos="this-is-a-long-string"//achar(0)

 write(0,*) 'example_fortran_api_append'
 c_ncolumns = ncolumns
 odb_handle = odb_write_new(config, cerr)
 odb_it = odb_write_iterator_new(odb_handle, outputfile, cerr);
 
 cerr = odb_write_set_no_of_columns(odb_it, ncolumns)
 if (cerr == 0) cerr = odb_write_set_column(odb_it, 0, ODB_INTEGER, "ifoo"//achar(0))
 if (cerr == 0) cerr = odb_write_set_column(odb_it, 1, ODB_REAL, "nbar"//achar(0))
 if (cerr == 0) cerr = odb_write_set_bitfield(odb_it, 2, ODB_BITFIELD, "status"//achar(0), &
                                              "active:passive:blacklisted:"//achar(0), &
                                              "1:1:4:"//achar(0))
 if (cerr == 0) cerr = odb_write_set_column(odb_it, 3, ODB_STRING, "wigos"//achar(0))
 if (cerr == 0) cerr = odb_write_set_column_size_doubles(odb_it, 3, 4);
 if (cerr == 0) cerr = odb_write_set_column(odb_it, 4, ODB_STRING, "expver"//achar(0))
 if (cerr == 0) cerr = odb_write_set_column(odb_it, 5, ODB_DOUBLE, "dbar"//achar(0))

 if (cerr == 0) cerr = odb_write_set_missing_value(odb_it, 0, 1.0_8)
 if (cerr == 0) cerr = odb_write_header(odb_it)

 if (cerr == 0) cerr = odb_write_get_row_buffer_size_doubles(odb_it, row_length_doubles)
 do i = 1, ncolumns
     if (cerr == 0) cerr = odb_write_get_column_offset(odb_it, i-1, offsets(i))
 enddo
 if (cerr /= 0) stop 1

 ! Sanity check!
 if (row_length_doubles /= 9) stop 1
 if (any(offsets /= (/1, 2, 3, 4, 8, 9/))) stop 1

 allocate(one_row(row_length_doubles))
 do i=1,10
   one_row(offsets(1)) = i
   one_row(offsets(2)) = i
   one_row(offsets(3)) = 5
   one_row(offsets(4):offsets(4)+3) = transfer(wigos, one_row(offsets(4):offsets(4)+3))
   one_row(offsets(5)) = transfer(expver, one_row(5))
   one_row(offsets(6)) = 5
   cerr = odb_write_set_next_row(odb_it, one_row, c_ncolumns)
   if (cerr /= 0) stop 1
 enddo
 deallocate(one_row)

 cerr = odb_write_iterator_delete(odb_it)

 if (cerr == 0) odb_it = odb_append_iterator_new(odb_handle, outputfile, cerr);
 
 if (cerr == 0) cerr = odb_write_set_no_of_columns(odb_it, ncolumns)
 if (cerr == 0) cerr = odb_write_set_column(odb_it, 0, ODB_INTEGER, "ifoo"//achar(0))
 if (cerr == 0) cerr = odb_write_set_column(odb_it, 1, ODB_REAL, "nbar"//achar(0))
 if (cerr == 0) cerr = odb_write_set_bitfield(odb_it, 2, ODB_BITFIELD, "status"//achar(0), &
                                     "active:passive:blacklisted:"//achar(0), &
                                     "1:1:4:"//achar(0))
 if (cerr == 0) cerr = odb_write_set_column(odb_it, 3, ODB_STRING, "wigos"//achar(0))
 if (cerr == 0) cerr = odb_write_set_column_size_doubles(odb_it, 3, 4);
 if (cerr == 0) cerr = odb_write_set_column(odb_it, 4, ODB_STRING, "expver"//achar(0))
 if (cerr == 0) cerr = odb_write_set_column(odb_it, 5, ODB_DOUBLE, "dbar"//achar(0))

 if (cerr == 0) cerr = odb_write_set_missing_value(odb_it, 0, 1.0_8)
 if (cerr == 0) cerr = odb_write_header(odb_it)

 if (cerr == 0) cerr = odb_write_get_row_buffer_size_doubles(odb_it, row_length_doubles)
 do i = 1, ncolumns
     if (cerr == 0) cerr = odb_write_get_column_offset(odb_it, i-1, offsets(i))
 enddo
 if (cerr /= 0) stop 1

 ! Sanity check!
 if (row_length_doubles /= 9) stop 1
 if (any(offsets /= (/1, 2, 3, 4, 8, 9/))) stop 1

 allocate(one_row(row_length_doubles))
 do i=1,10
   one_row(offsets(1)) = i
   one_row(offsets(2)) = i
   one_row(offsets(3)) = 5
   one_row(offsets(4):offsets(4)+3) = transfer(wigos, one_row(offsets(4):offsets(4)+3))
   one_row(offsets(5)) = transfer(expver, one_row(5))
   one_row(offsets(6)) = 5
   cerr = odb_write_set_next_row(odb_it, one_row, c_ncolumns)
   if (cerr /= 0) stop 1
 enddo
 deallocate(one_row)

 cerr = odb_write_iterator_delete(odb_it)
 if (cerr == 0) cerr = odb_write_delete(odb_handle)
 if (cerr /= 0) stop 1

end subroutine example_fortran_api_append

subroutine example_fortran_api_setup
 implicit none
 type(C_PTR)                                   :: odb_handle, odb_it
 integer(kind=C_INT)                           :: cerr
 character(kind=C_CHAR, len=max_varlen)        :: config = C_NULL_CHAR
 character(kind=C_CHAR, len=max_varlen)        :: outputfile="test.odb"//achar(0)
 integer(kind=4)                               :: i
 integer(kind=C_INT)                           :: itype, c_ncolumns 
 real(kind=C_DOUBLE), dimension(:), allocatable:: one_row
 integer(kind=c_int)                           :: offsets(ncolumns)
 integer(kind=c_int)                           :: row_length_doubles
 character(len=100)                            :: expver="fihn"//achar(0)
 character(len=100)                            :: wigos="this-is-a-long-string"//achar(0)

 write(0,*) 'example_fortran_api_setup'
 c_ncolumns = ncolumns
 odb_handle = odb_write_new(config, cerr)
 odb_it = odb_write_iterator_new(odb_handle, outputfile, cerr);
 
 cerr = odb_write_set_no_of_columns(odb_it, ncolumns)
 if (cerr == 0) cerr = odb_write_set_column(odb_it, 0, ODB_INTEGER, "ifoo"//achar(0))
 if (cerr == 0) cerr = odb_write_set_column(odb_it, 1, ODB_REAL, "nbar"//achar(0))
 if (cerr == 0) cerr = odb_write_set_bitfield(odb_it, 2, ODB_BITFIELD, "status"//achar(0), &
                                              "active:passive:blacklisted:"//achar(0), &
                                              "1:1:4:"//achar(0))
 if (cerr == 0) cerr = odb_write_set_column(odb_it, 3, ODB_STRING, "wigos"//achar(0))
 if (cerr == 0) cerr = odb_write_set_column_size_doubles(odb_it, 3, 4);
 if (cerr == 0) cerr = odb_write_set_column(odb_it, 4, ODB_STRING, "expver"//achar(0))
 if (cerr == 0) cerr = odb_write_set_column(odb_it, 5, ODB_DOUBLE, "dbar"//achar(0))

 if (cerr == 0) cerr = odb_write_set_missing_value(odb_it, 0, 1.0_8)
 if (cerr == 0) cerr = odb_write_header(odb_it)

 if (cerr == 0) cerr = odb_write_get_row_buffer_size_doubles(odb_it, row_length_doubles)
 do i = 1, ncolumns
     if (cerr == 0) cerr = odb_write_get_column_offset(odb_it, i-1, offsets(i))
 enddo
 if (cerr /= 0) stop 1

 ! Sanity check!
 if (row_length_doubles /= 9) stop 1
 if (any(offsets /= (/1, 2, 3, 4, 8, 9/))) stop 1

 allocate(one_row(row_length_doubles))
 do i=1,10
   one_row(offsets(1)) = i
   one_row(offsets(2)) = i
   one_row(offsets(3)) = 5
   one_row(offsets(4):offsets(4)+3) = transfer(wigos, one_row(offsets(4):offsets(4)+3))
   one_row(offsets(5)) = transfer(expver, one_row(5))
   one_row(offsets(6)) = 5
   cerr = odb_write_set_next_row(odb_it, one_row, c_ncolumns)
   if (cerr /= 0) stop 1
 enddo
 deallocate(one_row)

 cerr = odb_write_iterator_delete(odb_it)
 if (cerr == 0) cerr = odb_write_delete(odb_handle)
 if (cerr /= 0) stop 1

end subroutine example_fortran_api_setup

end program multio_replay_nemo_fapi
