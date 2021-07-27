program free_form
   print *, 'This statement starts in column 4 (with indentation)'
   x = 0.1; y = 0.7 ! Two small statements in one line 
                    ! Comment with an exclamation mark
   tan_x_plus_y = tan(x) + tan(y) / &  ! Line with continuation
           (1- tan(x)*tan(y))
end program free_form

