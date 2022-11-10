program pi_coarray

   use iso_fortran_env
   implicit none

   integer, parameter :: r15 = selected_real_kind(15)
   integer, parameter :: n = huge(1)
   real(kind=r15), parameter :: pi_ref = 3.1415926535897932384626433_real64

   integer :: i
   integer :: n_per_image[*]
   real(kind=r15) :: real_n[*]
   real(kind=r15) :: pi[*] = 0.0
   real(kind=r15) :: t[*]

   if (this_image() .eq. num_images()) then
      n_per_image = n/num_images()
      real_n = real(n_per_image*num_images(), r15)

      print *, 'Number of terms requested : ', n
      print *, 'Real number of terms      : ', real_n
      print *, 'Terms to compute per image: ', n_per_image
      print *, 'Number of images          : ', num_images()
      do i = 1, num_images() - 1
         n_per_image[i] = n_per_image
         real_n[i] = real(n_per_image*num_images(), r15)
      end do

   end if

   sync all

   ! Computed on each image
   do i = (this_image() - 1)*n_per_image, this_image()*n_per_image - 1
      t = (real(i) + 0.05)/real_n
      pi = pi + 4.0/(1.0 + t*t)
   end do

   sync all

   if (this_image() .eq. num_images()) then

      do i = 1, num_images() - 1
         !print *, pi[i]/real_n
         pi = pi + pi[i]
      end do

      print *, "Computed value", pi/real_n
      print *, "abs difference with reference", abs(pi_ref - pi/n)
   end if

end program pi_coarray

