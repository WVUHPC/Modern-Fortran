module mod_func

   use, intrinsic :: iso_fortran_env

   real(kind=real64), parameter :: f1_exact = 61.675091159487667993149630_real64
   real(kind=real64), parameter :: pi = 3.141592653589793238462643_real64

contains

   function f1(x)

      use, intrinsic :: iso_fortran_env

      implicit none

      real(kind=real64) :: f1
      real(kind=real64) :: x

      f1 = log(pi*x)*sin(pi*x)**2 + x

      return

   end function

   function f2(x)

      use, intrinsic :: iso_fortran_env

      implicit none

      real(kind=real64) :: f2
      real(kind=real64) :: x

      f2 = 50.0D+00/(pi*(2500.0D+00*x*x + 1.0D+00))

      return

   end function

end module mod_func

program main

   use, intrinsic :: iso_fortran_env
   use mod_func
   use mpi_f08

   implicit none

   integer(kind=int32)  :: i, ierror
   integer(kind=int32)  :: master
   integer(kind=int32)  :: p
   integer(kind=int32)  :: num_proc, rank
   integer(kind=int32)  :: n, my_n
   integer(kind=int32)  :: source, destin
   integer(kind=int32)  :: tag

   real(kind=real64)    :: a, b, my_a, my_b
   real(kind=real64)    :: error
   real(kind=real64)    :: total
   real(kind=real64)    :: wtime
   real(kind=real64)    :: x
   real(kind=real64)    :: my_total
   real(kind=real64)    :: exact
   type(MPI_Status)     :: mpistatus

   a = 1.0_real64
   b = 10.0_real64
   n = huge(1_int32)

   exact = f1_exact

   master = 0

   call MPI_Init(ierror)

   call MPI_Comm_size(MPI_COMM_WORLD, num_proc, ierror)

   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)

   ! RANK 0 reads in the quadrature rule, and parcels out the
   ! evaluation points among the processes.
   if (rank == 0) then

      ! We want N to be the total number of evaluations.
      ! If necessary, we adjust N to be divisible by the number of processors.
      my_n = n/(num_proc - 1)
      n = (num_proc - 1)*my_n

      wtime = MPI_Wtime()

      write (*, '(a)') ' '
      write (*, '(a)') '  Quadrature of f(x) from A to B.'
      write (*, '(a)') '  f(x) = log(pi*x)*sin(pi*x)**2 + x'
      write (*, '(a)') ' '
      write (*, '(a,g14.6)') '  A        = ', a
      write (*, '(a,g14.6)') '  B        = ', b
      write (*, '(a,i12)') '  N        = ', n
      write (*, '(a,g24.16)') '  Exact    = ', exact
      write (*, '(a)') ' '
   end if

   ! Rank 0 has the computed value for my_n
   source = master

   ! Share with all the ranks the number of elements that each rank will compute
   call MPI_Bcast(my_n, 1, MPI_INTEGER, source, MPI_COMM_WORLD, ierror)

   ! Rank 0 assigns each process a subinterval of [A,B].
   if (rank == 0) then

      do p = 1, num_proc - 1

         my_a = (real(num_proc - p, kind=real64)*a &
                 + real(p - 1, kind=real64)*b) &
                /real(num_proc - 1, kind=real64)

         destin = p
         tag = 1
         call MPI_Send(my_a, 1, MPI_DOUBLE_PRECISION, destin, tag, &
                       MPI_COMM_WORLD, ierror)

         my_b = (real(num_proc - p - 1, kind=real64)*a &
                 + real(p, kind=real64)*b) &
                /real(num_proc - 1, kind=real64)

         destin = p
         tag = 2
         call MPI_Send(my_b, 1, MPI_DOUBLE_PRECISION, destin, tag, &
                       MPI_COMM_WORLD, ierror)

      end do

      total = 0.0D+00
      my_total = 0.0D+00

!  Processes receive MY_A, MY_B, and compute their part of the integral.
   else

      source = master
      tag = 1

      call MPI_Recv(my_a, 1, MPI_DOUBLE_PRECISION, source, tag, &
                    MPI_COMM_WORLD, mpistatus, ierror)

      source = master
      tag = 2

      call MPI_Recv(my_b, 1, MPI_DOUBLE_PRECISION, source, tag, &
                    MPI_COMM_WORLD, mpistatus, ierror)

      my_total = 0.0D+00
      do i = 1, my_n
         x = (real(my_n - i, kind=real64)*my_a &
              + real(i - 1, kind=real64)*my_b) &
             /real(my_n - 1, kind=real64)
         my_total = my_total + f1(x)
      end do

      my_total = (my_b - my_a)*my_total/real(my_n, kind=real64)

      write (*, '(a,i3,a,g14.6)') &
         '  RANK: ', rank, ' Partial Quadrature: ', my_total

   end if

   ! Each process sends its value of MY_TOTAL to the master process, to
   ! be summed in TOTAL.
   call MPI_Reduce(my_total, total, 1, MPI_DOUBLE_PRECISION, &
                   MPI_SUM, master, MPI_COMM_WORLD, ierror)

   ! Report the results.
   if (rank == master) then

      error = abs(total - exact)
      wtime = MPI_Wtime() - wtime

      write (*, '(a)') ' '
      write (*, '(a,g24.16)') '  Estimate = ', total
      write (*, '(a,g14.6)') '  Error    = ', error
      write (*, '(a,g14.6)') '  Time     = ', wtime

   end if

!  Terminate MPI.
   call MPI_Finalize(ierror)

!  Terminate.
   if (rank == master) then
      write (*, '(a)') ' '
      write (*, '(a)') '  Normal end of execution.'
   end if

   stop

end program
