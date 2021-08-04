program cob

   implicit none
   character(len=10) :: i[-2:2, 2, 1:*]

   if (this_image() .eq. num_images()) then
      write (*, *) "this_image()", this_image()
      write (*, *) "this_image( i )", this_image(i)
      write (*, *) "lcobound( i )", lcobound(i)
      write (*, *) "ucobound( i )", ucobound(i)
      write (*, *) "image_index(ucobound(i))", image_index(i, ucobound(i))
   end if

end program cob
