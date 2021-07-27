program synch
   implicit none
   include 'mpif.h'

   integer my_pe_num, errcode, numbertoreceive, numbertosend
   integer index, result
   integer status(MPI_STATUS_SIZE)

   call MPI_INIT(errcode)
   call MPI_COMM_RANK(MPI_COMM_WORLD, my_pe_num, errcode)
   numbertosend = 4
   result = 0

   print *, 'FIRST COMMUNICATION'
   if (my_PE_num .EQ. 0) then
      do index = 1, my_pe_num - 1
         call MPI_Send(numbertosend, 1, MPI_INTEGER, index, 10, MPI_COMM_WORLD, errcode)
      end do

   else

      call MPI_Recv(numbertoreceive, 1, MPI_INTEGER, 0, 10, MPI_COMM_WORLD, status, errcode)
      result = numbertoreceive*my_PE_num

   end if

   print *, 'PRINTING'
   do index = 0, my_pe_num - 1
      call MPI_Barrier(MPI_COMM_WORLD, errcode)
      print *, 'RANK:', index, errcode
      if (my_PE_num .EQ. index) then
         write (0, *) 'PE ', my_PE_num, 's result is ', result, '.'
      end if
   end do

   print *, 'TOTAL'
   if (my_PE_num .EQ. 0) then
      do index = 1, my_pe_num - 1
         call MPI_Recv(numbertoreceive, 1, MPI_INTEGER, index, 10, MPI_COMM_WORLD, status, errcode)
         result = result + numbertoreceive
      end do
      print *, 'Total is ', result, '.'

   else

      call MPI_Send(result, 1, MPI_INTEGER, 0, 10, MPI_COMM_WORLD, errcode)

   end if

   call MPI_FINALIZE(errcode)

end program
