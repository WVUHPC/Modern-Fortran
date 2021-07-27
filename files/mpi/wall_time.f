! wall_clock.f -*-f90-*-
! Gist: https://gist.github.com/t-nissie/e317fd796d4b885106139f752ee4300d
!!
program wall_clock
  implicit none
  include 'mpif.h'
  integer i_error, mpi_my_rank
  real*8  start, finish
  call MPI_INIT(i_error)
  call MPI_COMM_RANK(MPI_COMM_WORLD, mpi_my_rank, i_error)
  call MPI_Barrier(  MPI_COMM_WORLD, i_error)
  start  = MPI_Wtime()
  call sleep(3)
  call MPI_Barrier(  MPI_COMM_WORLD, i_error)
  finish = MPI_Wtime()
  write(6,'(i3,f7.4)') mpi_my_rank, finish - start
  call MPI_Finalize(i_error)
end program wall_clock
!Local variables:
!  compile-command: "mpif90 -ffree-form -o wall_clock wall_clock.f && mpiexec -np 4 ./wall_clock"
!End:


