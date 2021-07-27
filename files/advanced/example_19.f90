module my_module

   implicit none

   real :: d_default = -1.125

contains

   subroutine calc(a, b, c, d, e)
      real :: a, b, c, e
      real, optional :: d
      real :: dd
      if (present(d)) then
         dd = d
      else
         print *, 'd argument not present, using default'
         dd = d_default
      end if

      print '(5(F7.3))', a,b,c,dd,e

   end subroutine

end module

program main

   use my_module

   implicit none

   call calc(1., 2., 3., 4., 5.)
   call calc(1., 2., 3., e=5.)
   call calc(a=1., b=2., c=3., d=4., e=5.)
   call calc(b=2., d=4., a=1., c=3., e=5.)
   call calc(1., 2., 3., d=4., e=5.)
   call calc(1., 2., d=4., c=3., e=5.)

end program
