program main

   integer(kind=4) :: i = 9, j
   real(kind=8) :: x = 3.14, y, z
   complex(kind=4) :: sec = (-1, 1)
   character(len=20, kind=1) :: word = "Electron"
   logical(kind=1) :: is_good, is_bad

   print *, 'Integer   : ', i
   print *, 'Real      : ', x
   print *, 'Complex   : ', sec
   print *, 'Character : ', word
   print *, 'Logical   : ', is_good

end program
