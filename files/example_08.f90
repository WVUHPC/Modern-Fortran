program kinds

   use iso_fortran_env

   implicit none

   integer :: i, my_kind

   real :: x_real              ! the default
   real*4 :: x_real4           ! Real with 4 bytes
   real*8 :: x_real8           ! Real with 8 bytes
   DOUBLE PRECISION :: x_db    ! Old way from FORTRAN 66

   integer, parameter :: k9 = selected_real_kind(9)
   real(kind=k9) :: r
 
   print *, 'Kind for integer            :', kind(i)
   print *, 'Kind for real               :', kind(x_real)
   print *, 'Kind for real*4             :', kind(x_real4)
   print *, 'Kind for real*8             :', kind(x_real8)
   print *, 'Kind for DOUBLE PRECISION   :', kind(x_db)
   print *, ''

   my_kind = selected_real_kind(9)
   print *, 'Which is the "kind" I should use to get 9 significant digits?  ', my_kind

   my_kind = selected_real_kind(15)
   print *, 'Which is the "kind" I should use to get 15 significant digits? ', my_kind

   r = 2._k9;
   print *, 'Value for k9', k9 
   print *, 'Square root of 2.0 for default real      :', sqrt(2.0)   ! prints 1.41421354
   print *, 'Square root of 2.0 for DOUBLE PRECISION  :', sqrt(2.0d0) ! prints 1.41421354
   print *, 'Square root of 2.0 for numer of kind(k9) :', sqrt(r)     ! prints 1.4142135623730951

end program

