
subroutine sub_one(ix, oy, ioz)
   real, intent(in) :: ix ! input
   real, intent(out)  :: oy ! output
   real, intent(inout) :: ioz ! input and output

   oy = sqrt(real(ix)) + ioz
   ioz = max(ix, oy)

end subroutine

function func(i) result(j)
   integer, intent(in) :: i ! input
   integer              :: j ! output

   j = sqrt(real(i)) + log(real(i))
end function

program main

   implicit none

   integer :: i
   integer :: func
   real :: ix, oy, ioz

   ix = 10.0
   ioz = 20.0

   i = huge(1)
   print *, "i=", i, char(10), " sqrt(i) + log(i) =", func(i)

   print *, 'Before:', ix, oy, ioz
   call sub_one(ix, oy, ioz)
   print *, 'After :', ix, oy, ioz

end program
