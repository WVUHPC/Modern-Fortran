      PROGRAM square
      DO 100 I=1,100
      X=I
      X2=X*X
      IF(X2.LT.100) print *, 'X=', I, ' X^2 wil have less than 3 digits'
100   CONTINUE
      END

