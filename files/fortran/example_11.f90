program main

   implicit none

   integer :: i
   character(len=1024) :: arg
   integer :: length, istatus
   real :: a(3)

   print *, 'Number of arguments', command_argument_count()

   do i = 1, command_argument_count()

      call get_command_argument(i, arg, length, istatus)

      print *, trim(arg)

   end do

   if (command_argument_count() == 3) then

      do i = 1, 3
         call get_command_argument(i, arg, length, istatus)
         read (arg, *) a(i)
      end do

   end if

   print '(3(e12.3))', a(:)

end program
