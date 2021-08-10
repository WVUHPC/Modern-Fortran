program main

   integer :: i

   call update(1)

   call update(2)

contains

   subroutine update(i)

      integer :: i
      real, dimension(:), allocatable, save :: a

      if (i == 1) then

         print *, 'Creating first array'
         allocate (a(10))

         a = 1.23

      else
         print *, 'Reusing array'
         print *, a(5)

      end if

   end subroutine

end program main

