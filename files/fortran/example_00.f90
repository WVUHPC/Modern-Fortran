program converge_pi

   use iso_fortran_env

   implicit none

   integer, parameter :: knd = max(selected_real_kind(15), selected_real_kind(33))
   integer, parameter :: n = huge(1)/1000
   real(kind=knd), parameter :: pi_ref = 3.1415926535897932384626433832795028841971_knd

   integer :: i
   real(kind=knd) :: pi = 0.0, t

   print *, 'KIND      : ', knd
   print *, 'PRECISION : ', precision(pi)
   print *, 'RANGE     : ', range(pi)

   do i = 0, n - 1
      t = real(i + 0.5, kind=knd)/n
      pi = pi + 4.0*(1.0/(1.0 + t*t))
   end do

   print *, ""
   print *, "Number of terms in the series : ", n
   print *, "Computed value : ", pi/n
   print *, "Reference vaue : ", pi_ref
   print *, "ABS difference with reference : ", abs(pi_ref - pi/n)

end program converge_pi
