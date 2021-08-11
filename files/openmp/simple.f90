program simple

integer,parameter :: n=100000
integer :: array(n)

print *,n
!$omp parallel do
do i = 1, n
   array(i) = sqrt(real(i))
enddo
!$omp end parallel do

end program

