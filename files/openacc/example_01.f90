module process_mod
contains
   subroutine process(a, b, n)
      real :: a(:), b(:)
      integer :: n, i

      print *, "subroutine process..."
!$ACC PARALLEL LOOP
      do i = 1, n
         b(i) = exp(sin(a(i)))
      end do
!$ACC END PARALLEL LOOP

   end subroutine
end module process_mod

program simple

   use process_mod

   integer, parameter :: n = 1*10**9
   integer :: i
   real, allocatable:: a(:), b(:)
   allocate (a(n), b(n))

   print *, "initializing array..."
!$ACC PARALLEL LOOP
   do i = 1, n
      a(i) = asin(log(real(i)))
   end do
!ACC END PARALLEL LOOP

   call process(a, b, n)

end program simple

