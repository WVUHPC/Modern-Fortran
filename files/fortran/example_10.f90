program main

   implicit none

   integer :: i
   character(len=1024) :: arg
   integer :: length, istatus

   print *, 'Number of arguments', command_argument_count()

   do i = 1, command_argument_count()

      call get_command_argument(i, arg, length, istatus)

      print *, trim(arg)

   end do

end program
