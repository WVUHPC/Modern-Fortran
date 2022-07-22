program group
   use mpi_f08

   implicit none

   integer :: nsize, i
   integer :: rank, new_rank, sendbuf, recvbuf, numtasks, ierr
   integer, dimension(:), allocatable :: ranks1(:), ranks2(:)
   type(MPI_Group) :: orig_group, new_group  ! MPI 3.0
   type(MPI_Comm) :: new_comm

   call MPI_INIT(ierr)
   call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
   call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)

   if (mod(numtasks, 2) /= 0) then
      print *, 'Enter a even number of processes '
      call MPI_FINALIZE(ierr)
      stop
   end if

   allocate (ranks1(numtasks/2))
   allocate (ranks2(numtasks/2))

   ranks1 = [(i, i=0, size(ranks1) - 1)]
   ranks2 = [(size(ranks1) + i, i=0, size(ranks2) - 1)]

   sendbuf = rank

! extract the original group handle
   call MPI_COMM_GROUP(MPI_COMM_WORLD, orig_group, ierr)

! divide tasks into two distinct groups based upon rank
   if (rank < numtasks/2) then
      call MPI_GROUP_INCL(orig_group, numtasks/2, ranks1, new_group, ierr)
   else
      call MPI_GROUP_INCL(orig_group, numtasks/2, ranks2, new_group, ierr)
   end if

! create new new communicator and then perform collective communications
   call MPI_COMM_CREATE(MPI_COMM_WORLD, new_group, new_comm, ierr)
   call MPI_ALLREDUCE(sendbuf, recvbuf, 1, MPI_INTEGER, MPI_SUM, new_comm, ierr)

! get rank in new group
   call MPI_GROUP_RANK(new_group, new_rank, ierr)
   print *, 'rank= ', rank, ' newrank= ', new_rank, ' recvbuf= ', recvbuf

   call MPI_FINALIZE(ierr)
end program

