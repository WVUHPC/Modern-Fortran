module time_mgt

contains

   subroutine timestamp()

      use, intrinsic :: iso_fortran_env

      implicit none

      character(len=8) :: ampm
      integer(kind=int32) :: d
      integer(kind=int32) :: h
      integer(kind=int32) :: m
      integer(kind=int32) :: mm
      character(len=9), parameter, dimension(12) :: month = (/ &
                                                    'January  ', 'February ', &
                                                    'March    ', 'April    ', &
                                                    'May      ', 'June     ', &
                                                    'July     ', 'August   ', &
                                                    'September', 'October  ', &
                                                    'November ', 'December '/)
      integer(kind=4) :: n
      integer(kind=4) :: s
      integer(kind=4) :: values(8)
      integer(kind=4) :: y

      call date_and_time(values=values)

      y = values(1)
      m = values(2)
      d = values(3)
      h = values(5)
      n = values(6)
      s = values(7)
      mm = values(8)

      if (h < 12) then
         ampm = 'AM'
      else if (h == 12) then
         if (n == 0 .and. s == 0) then
            ampm = 'Noon'
         else
            ampm = 'PM'
         end if
      else
         h = h - 12
         if (h < 12) then
            ampm = 'PM'
         else if (h == 12) then
            if (n == 0 .and. s == 0) then
               ampm = 'Midnight'
            else
               ampm = 'AM'
            end if
         end if
      end if

      write (*, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)') &
         d, trim(month(m)), y, h, ':', n, ':', s, '.', mm, trim(ampm)

      return
   end subroutine

end module

program main

   use, intrinsic :: iso_fortran_env
   use time_mgt
   use mpi_f08

   implicit none

   integer, parameter :: n=1000000
   integer(kind=int32) :: i
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

      call timestamp()
      ! Only rank = 0 print this
      write (*, '(a)') ''
      write (*, '(a,i2,2x,a)') 'RANK:', rank, ' Master process reporting:'
      write (*, '(a,i2,2x,a,i3)') 'RANK:', rank, &
         ' The number of MPI processes is ', num_proc

   else

      ! Every MPI process will print this message.
      write (*, '(a,i2,2x,a,i8)') 'RANK:', rank, &
      ' Allocating array of size:', rank*n

      ! Each rank will allocate an array of a different size
      allocate (array(rank*n))
      do i = 1, size(array)
         array(i) = log(real(rank))+sqrt(real(i))
      end do

      ! Reporting sum of array
      write (*, '(a,i2,2x,a,e12.3)') 'RANK:', rank, ' Sum of array:', sum(array)

   end if

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

      call timestamp()
   end if

   stop

end program

