program main

   integer, parameter :: r9 = selected_real_kind(9, 99)

   print *, 'Information about : selected_real_kind(9, 99)'
   print *, 'KIND      : ', kind(1.1_r9)
   print *, 'PRECISION : ', precision(1.1_r9)
   print *, 'RANGE     : ', range(1.1_r9)

end program

