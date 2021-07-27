module mod_ring

contains

   subroutine ring(num_proc, rank)

      use, intrinsic :: iso_fortran_env
      use mpi_f08

      implicit none

      integer(kind=int32), intent(in) :: num_proc
      integer(kind=int32), intent(in) :: rank

      integer(kind=int32), parameter :: n_test_num = 7
      integer(kind=int32), parameter :: test_num = 20
      integer(kind=int32), parameter :: decimal_precision = 5
      integer(kind=int32), parameter :: exponent_range = 300
      integer(kind=int32), parameter :: dp = &
                                        selected_real_kind(decimal_precision, exponent_range)

      integer(kind=int32) :: source, destin
      integer(kind=int32) :: ierror
      integer(kind=int32) :: i, j, n
      integer(kind=int32), dimension(n_test_num) :: n_test
      integer(kind=int32) :: test
      real(kind=real64)   :: tave, tmax, tmin
      real(kind=real64)   :: wtime
      real(kind=dp), allocatable, dimension(:) :: x

      !type(MPI_Status)   :: mpistatus(MPI_STATUS_SIZE)
      type(MPI_Status)   :: mpistatus
      type(MPI_Datatype) :: realtype

      call MPI_Type_Create_F90_real(decimal_precision, exponent_range, &
                                    realtype, ierror)
      !if (ierror == MPI_SUCCESS) print *, 'MPI_Type_Create_F90_real'

      !real(kind=real64), allocatable :: x(:)

      ! Old way of creating arrays
      !n_test = (/ 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000 /)
      n_test = [(10**i, i=1, n_test_num)]

      if (rank == 0) then
         write (*, '(a)') ' '
         write (*, '(a,i3)') ' RANK 0: Number of cases:    ', test_num
         write (*, '(a,i3)') ' RANK 0: Arrays of KIND:     ', real64
         write (*, '(a,i3)') ' RANK 0: Number of processes:', num_proc
         write (*, '(a)') ' '
         write (*, '(a11,a14,a14,a14)') 'N', 'T min', 'T max', 'T ave'
         write (*, '(a)') ' '
      end if

      ! Testing with arrays of increasing size
      do i = 1, n_test_num

         n = n_test(i)

         allocate (x(1:n))

         ! RANK 0 sends very first message,
         ! then waits to receive the "echo" that has gone around the world.
         if (rank == 0) then

            destin = 1
            source = num_proc - 1

            tave = 0.0D+00
            tmin = huge(1.0D+00)
            tmax = 0.0D+00

            ! Loop to collect statistics of time to cycle
            do test = 1, test_num
               ! Set the entries of X in a way that identifies the test
               ! There is an implicit loop to populate the array
               x = [(real(test + j - 1, kind=real64), j=1, n)]

               wtime = MPI_Wtime()
               call MPI_Send(x, n, realtype, destin, 0, MPI_COMM_WORLD, ierror)
               !if (ierror == MPI_SUCCESS) print *, 'MPI_Send'

               call MPI_Recv(x, n, realtype, source, 0, MPI_COMM_WORLD, mpistatus, ierror)
               !if (ierror == MPI_SUCCESS) print *, 'MPI_Recv'

               wtime = MPI_Wtime() - wtime

               ! Record the time it took.
               tave = tave + wtime
               tmin = min(tmin, wtime)
               tmax = max(tmax, wtime)

            end do

            tave = tave/real(test_num, kind=real64)

            write (*, '(2x,i9,2x,f12.7,2x,f12.7,2x,f12.7)') n, tmin, tmax, tave

            ! All ranks > 0: Receive first from rank-1,
            ! then send to rank+1 or 0 if it is the last rank
         else

            source = rank - 1
            destin = mod(rank + 1, num_proc)

            ! Loop to collect statistics of time to cycle
            do test = 1, test_num
               ! ---> Recv ++++ Send --->
               call MPI_Recv(x, n, realtype, source, 0, MPI_COMM_WORLD, mpistatus, ierror)
               !if (ierror == MPI_SUCCESS) print *, 'MPI_Recv'

               call MPI_Send(x, n, realtype, destin, 0, MPI_COMM_WORLD, ierror)
               !if (ierror == MPI_SUCCESS) print *, 'MPI_Send'

            end do

         end if

         deallocate (x)

      end do

      return
   end

end module

program main

   use, intrinsic :: iso_fortran_env
   use mpi_f08
   use mod_ring

   implicit none

   integer(kind=int32) :: ierror
   integer(kind=int32) :: rank
   integer(kind=int32) :: num_proc

   ! First MPI call
   call MPI_Init(ierror)
   !if (ierror == MPI_SUCCESS) print *, 'MPI_Init'

   ! Get the number of processes (num_proc)
   call MPI_Comm_size(MPI_COMM_WORLD, num_proc, ierror)
   !if (ierror == MPI_SUCCESS) print *, 'MPI_Comm_size'

   ! Get the individual process (rank)
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)
   !if (ierror == MPI_SUCCESS) print *, 'MPI_Comm_rank'

   if (rank == 0) then
      write (*, '(a)') ' '
      write (*, '(a)') ' RANK 0: MPI prepared for ring of messages'
   end if

   ! Routine that will transfer messages in a ring
   call ring(num_proc, rank)

   ! Last MPI call
   call MPI_Finalize(ierror)
   !if (ierror == MPI_SUCCESS) print *, 'MPI_Finalize'

   ! Terminate.
   if (rank == 0) then
      write (*, '(a)') ' '
      write (*, '(a)') ' RANK 0: Execution complete'
      write (*, '(a)') ' '
   end if

   stop
end

