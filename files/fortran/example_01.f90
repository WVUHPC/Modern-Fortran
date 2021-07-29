program main

   ! This is pi defined as a constant
   ! Most decimals will be ignored as the default precision
   ! for a single precision real data type only provides 6 to 9 
   ! significant decimals
   real, parameter :: pi = 3.14159265358979323846264338327950288419716&
                          &9399375105820974944592307816406286208998628&
                          ! This comment works here and does not interrupt
                          ! the continuation line
                          &0348253421170679821480865132823066470938446&
                          ! In standard Fortran 77, anything 
                          ! beyond column 72 is ignored.
                          ! In modern Fortran the limit is 132 allowing very long lines like this one
                          &09550582231725359408128481117450284102701938521105559644622948954930381964428810975665933446128475648233&
                          ! Also comments does not count to the limit of 255 continuation lines.
                          &786783165271201909145648566923460348610454326648213393607260249141273

   print *, pi

end program
