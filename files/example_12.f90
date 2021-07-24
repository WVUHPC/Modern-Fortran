program arraymem
   
   use iso_fortran_env

   implicit none

   integer, parameter :: n = 2*1024*1024 - 4096

   print *, 'Numeric Storage (bits): ', numeric_storage_size
   print *, 'Creating array N ', n

   call meanArray(n)

contains

   subroutine meanArray(n)
      integer, intent(in) :: n
      integer :: dumb
      integer, dimension(n) :: a
      a = 1
      print *, sum(a)
      read *, dumb
   end subroutine meanArray

end program arraymem
