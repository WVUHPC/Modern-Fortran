module thetables

   integer, parameter :: i6 = selected_int_kind(6)
   integer, parameter :: r9 = selected_real_kind(9)

contains

   subroutine prime_table(prime_num, primes)

      implicit none

      integer(kind=i6) prime_num

      integer(kind=i6) i
      integer(kind=i6) j
      integer(kind=i6) p
      logical prime
      integer(kind=i6) primes(prime_num)

      i = 2
      p = 0

      do while (p < prime_num)

         prime = .true.

         do j = 2, i - 1
            if (mod(i, j) == 0) then
               prime = .false.
               exit
            end if
         end do

         if (prime) then
            p = p + 1
            primes(p) = i
         end if

         i = i + 1

      end do

      return
   end

   subroutine sine_table(sine_num, sines)

      implicit none

      integer(kind=i6) sine_num

      real(kind=r9) a
      integer(kind=i6) i
      integer(kind=i6) j
      real(kind=r9), parameter :: r8_pi = 3.141592653589793D+00
      real(kind=r9) sines(sine_num)

      do i = 1, sine_num
         sines(i) = 0.0D+00
         do j = 1, i
            a = real(j - 1, kind=r9)*r8_pi/real(sine_num - 1, kind=r9)
            sines(i) = sines(i) + sin(a)
         end do
      end do

      return
   end

   subroutine cosine_table(cosine_num, cosines)

      implicit none

      integer(kind=i6) cosine_num

      real(kind=r9) a
      integer(kind=i6) i
      integer(kind=i6) j
      real(kind=r9), parameter :: r8_pi = 3.141592653589793D+00
      real(kind=r9) cosines(cosine_num)

      do i = 1, cosine_num
         cosines(i) = 0.0D+00
         do j = 1, i
            a = real(j - 1, kind=r9)*r8_pi/real(cosine_num - 1, kind=r9)
            cosines(i) = cosines(i) + sin(a)
         end do
      end do

      return
   end

end module thetables

program main

   use omp_lib
   use thetables

   implicit none

   integer(kind=i6) prime_num
   integer(kind=i6), allocatable :: primes(:)
   integer(kind=i6) sine_num, cosine_num
   real(kind=r9), allocatable :: sines(:), cosines(:)
   real(kind=r9) wtime
   real(kind=r9) wtime1, wtime2, wtime3
  

   write (*, '(a)') ' '
   write (*, '(a)') 'MULTITASK_OPENMP:'
   write (*, '(a)') '  FORTRAN90/OpenMP version'
   write (*, '(a)') '  Demonstrate how OpenMP can "multitask" by using the'
   write (*, '(a)') &
      '  SECTIONS directive to carry out several tasks in parallel.'

   prime_num = 20000
   allocate (primes(1:prime_num))
   sine_num = 20000
   cosine_num = 40000
   allocate (sines(1:sine_num))
   allocate (cosines(1:cosine_num))

   wtime = omp_get_wtime()

!$omp parallel shared ( prime_num, primes, sine_num, sines )

   !$omp sections

   !$omp section
   wtime1 = omp_get_wtime()
   call prime_table(prime_num, primes)
   wtime1 = omp_get_wtime() - wtime1
   !$omp section
   wtime2 = omp_get_wtime()
   call sine_table(sine_num, sines)
   wtime2 = omp_get_wtime() - wtime2
   !$omp section
   wtime3 = omp_get_wtime()
   call cosine_table(cosine_num, cosines)
   wtime3 = omp_get_wtime() - wtime3 
   !$omp end sections

!$omp end parallel

   wtime = omp_get_wtime() - wtime

   write (*, '(a)') ' '
   write (*, '(a,i6)') '  Number of primes computed was ', prime_num
   write (*, '(a,i12)') '  Last prime was ', primes(prime_num)
   write (*, '(a,i6)') '  Number of sines computed was ', sine_num
   write (*, '(a,g14.6)') '  Last sine computed was ', sines(sine_num)
   write (*, '(a,i6)') '  Number of cosines computed was ', cosine_num
   write (*, '(a,g14.6)') '  Last cosine computed was ', cosines(cosine_num)
   write (*, '(a)') ' '
   write (*, '(a,g14.6)') '  Elapsed time = ', wtime
   write (*, '(a,g14.6)') '  Task 1 time = ', wtime1
   write (*, '(a,g14.6)') '  Task 2 time = ', wtime2
   write (*, '(a,g14.6)') '  Task 3 time = ', wtime3
!
!  Free memory.
!
   deallocate (primes)
   deallocate (sines)
   deallocate (cosines)
!
!  Terminate.
!
   write (*, '(a)') ' '
   write (*, '(a)') 'MULTITASK_OPENMP:'
   write (*, '(a)') '  Normal end of execution.'
   write (*, '(a)') ' '

   stop
end

