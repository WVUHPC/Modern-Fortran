program clauses

   implicit none
   integer, parameter :: n=10
   integer :: i, a, b, c, d
   character(len=*), parameter :: layout_1 = "(a, 4(a, i12))"
   character(len=*), parameter :: layout_2 = "(a, i4, 4(a, i12))"

   a = 1
   b = 1
   c = 1
   d = 1

   print layout_1, "BEFORE", " SHARED:", a, " PRIVATE:", b, " FIRSTPRIVATE:", c, " LASTPRIVATE:", d

!$OMP PARALLEL DO SHARED(a) PRIVATE(b) FIRSTPRIVATE(c) LASTPRIVATE(d)
   do i = 1, n

      if (n<11) print layout_2, "I=", i, " SHARED:", a, " PRIVATE:", b, " FIRSTPRIVATE:", c, " LASTPRIVATE:", d
      a = a + i
      b = b + i
      c = c + i
      d = d + i
   end do
!$OMP END PARALLEL DO

   print layout_1, "AFTER ", " SHARED:", a, " PRIVATE:", b, " FIRSTPRIVATE:", c, " LASTPRIVATE:", d

end program clauses
