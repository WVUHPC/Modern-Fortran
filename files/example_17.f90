module module_privs

   implicit none

   integer, parameter, private :: sp = selected_real_kind(6, 37)
   integer, parameter, private :: dp = selected_real_kind(15, 307)
   integer, parameter, private :: qp = selected_real_kind(33, 4931)

   real(kind=sp), protected :: pi_sigl = &
   3.141592653589793238462643383279502884197169399375105820974944592307&
   &8164062862089986280348253421170679821480865132823066470938446_sp
   real(kind=dp), protected :: pi_dble = &
   3.141592653589793238462643383279502884197169399375105820974944592307&
   &8164062862089986280348253421170679821480865132823066470938446_dp
   real(kind=qp), protected :: pi_quad = &
   3.141592653589793238462643383279502884197169399375105820974944592307&
   &8164062862089986280348253421170679821480865132823066470938446_qp
   real :: x = 50.25
   real, protected :: x_prot = 512.125
   real :: y = 3.0

contains

   subroutine show_pi_3()
      print *, 'PI with  6 digits', kind(pi_sigl), pi_sigl
      print *, 'PI with 15 digits', kind(pi_dble), pi_dble
      print *, 'PI with 33 digits', kind(pi_quad), pi_quad
   end subroutine show_pi_3

end module module_privs

program main

   use module_privs, my_y => y

   implicit none

   call show_pi_3()

   print *, 'x from inside the module : ', x
   x = 25.50
   print *, 'x changed outside module : ', x

   print *, 'x_prot:', x_prot
   ! This variable is protected and cannot be changed
   ! Uncommenting the line below will not compile
   !x_prot = 125.512
   print *, 'x_prot:', x_prot

   ! The variable 'y' is not visible as it was renamed
   !print *, y
   print *, 'my_y:', my_y

end program main

