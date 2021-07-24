program newkinds

   use iso_fortran_env

   implicit none

   real(kind=real32) :: x32
   real(kind=real64) :: x64
   real(kind=real128) :: x128

   print *, 'real32  : ', real32, achar(10), &
           ' real64  : ', real64, achar(10), &
           ' real128 : ', real128

   x32 = 2.0
   x64 = 2.0
   x128 = 2.0

   print *, 'SQRT(2.0) using kind=real32  :', sqrt(x32)
   print *, 'SQRT(2.0) using kind=real64  :', sqrt(x64)
   print *, 'SQRT(2.0) using kind=real128 :', sqrt(x128)

end program

