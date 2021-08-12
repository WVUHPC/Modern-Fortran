program scatter
   use mpi_f08

   integer, parameter :: nsize = 8
   integer :: numtasks, rank, sendcount, recvcount, source, ierr
   real, allocatable ::  sendbuf(:, :), recvbuf(:)

   call MPI_INIT(ierr)
   call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
   call MPI_COMM_size(MPI_COMM_WORLD, numtasks, ierr)

   allocate (sendbuf(nsize, numtasks))
   allocate (recvbuf(nsize))
   sendbuf = reshape([(i, i=1, nsize*numtasks)], [nsize, numtasks])

   if (rank == 0) then
      print *, ''
      print *, 'Original array to be distributed:'
      do i = 1, numtasks
         print *, sendbuf(:, i)
      end do
      print *, ''
   end if

   call MPI_BARRIER(MPI_COMM_WORLD)

   source = 1
   sendcount = nsize
   recvcount = nsize
   call MPI_SCATTER(sendbuf, sendcount, MPI_REAL, recvbuf, recvcount, MPI_REAL, &
                    source, MPI_COMM_WORLD, ierr)

   print *, 'rank= ', rank, ' recvbuf: ', recvbuf

   call MPI_FINALIZE(ierr)

   deallocate (sendbuf, recvbuf)

end program

