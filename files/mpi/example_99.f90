program main

   use, intrinsic :: iso_fortran_env
   use mpi_f08

   implicit none

   integer, parameter :: n = 1000000
   integer(kind=int32) :: i, j
   integer(kind=int32) :: ierror ! To control errors in MPI calls
   integer(kind=int32) :: rank   ! Unique number received by each process
   integer(kind=int32) :: num_proc ! Total number of processes
   real(kind=real64)   :: wtime
   real(kind=real64), allocatable, dimension(:) :: array

   ! Initialize MPI. This must be the first MPI call
   call MPI_Init(ierror)

   ! Get the number of processes
   call MPI_Comm_size(MPI_COMM_WORLD, num_proc, ierror)

   ! Get the individual process rank
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)

   if (rank == 0) then
      wtime = MPI_Wtime()

      ! Only rank = 0 print this
      write (*, '(a)') ''
      write (*, '(a,i2,2x,a)') 'RANK:', rank, ' Master process reporting:'
      write (*, '(a,i2,2x,a,i3)') 'RANK:', rank, &
         ' The number of MPI processes is ', num_proc
   end if

   do j = 0, num_proc - 1

     if (rank == j) then
      call MPI_Barrier(MPI_COMM_WORLD, ierror)
      print *, 'RANK', rank, 'Reached barrier'
 
         if (rank /= 0) then

            ! Every MPI process will print this message.
            write (0, '(a,i2,2x,a,i8)') 'RANK:', rank, &
               ' Allocating array of size:', rank*n

            ! Each rank will allocate an array of a different size
            allocate (array(rank*n))
            do i = 1, size(array)
               array(i) = log(real(rank)) + sqrt(real(i))
            end do

            ! Reporting sum of array
            write (0, '(a,i2,2x,a,e12.3)') 'RANK:', rank, ' Sum of array:', sum(array)
            deallocate (array)

         end if

      end if

   end do

   if (rank == 0) then
      write (*, '(a)') ''
      write (*, '(a,i2,2x,a)') 'RANK:', rank, ' Master process reporting:'
      write (*, '(a,i2,2x,a)') 'RANK:', rank, &
         ' Normal end of execution for master'

      wtime = MPI_Wtime() - wtime
      write (*, '(a)') ''
      write (*, '(a,i2,2x,a,g14.6,a)') &
         'RANK:', rank, ' Elapsed wall clock time = ', wtime, ' seconds.'
      write (*, '(a)') ''

   end if

   ! No more MPI calls after Finalize
   call MPI_Finalize(ierror)

   ! Ranks are intrinsic to each process and this conditional is legal
   if (rank == 0) then
      write (*, '(a)') ''
      write (*, '(a,i2,2x,a)') 'RANK:', rank, ' Master process reporting:'
      write (*, '(a,i2,2x,a)') 'RANK:', rank, ' Normal end of execution for all'
   end if

   stop

end program

