       REAL FUNCTION HG3(MU1, MU2, COSDAZ, G)

C      This is the HG (Henyey Greenstein) phase function, except it
C      uses three cosine arguments as well as asymmetry

C      Arguments
       REAL MU1    ! cosine of satzen
       REAL MU2    ! cosine of solzen
       REAL COSDAZ ! cosine of (solazi - satazi)
       REAL G      ! asymmetry
C
C      Local variables
       REAL G2
       REAL MU0
c       REAL NORMB
       REAL X
       REAL X1
       REAL X2


C      ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
       G2=G*G
C      mu0 = cos(theta = A - B) = cosAcosB + sinAsinB
c      mu1=cosA and mu2=cosB, and  sinX=sqrt( 1 - (cosX)^2 )
ccc
c       MU0 = MU1*MU2 + SQRT(1 - MU1*MU1)*SQRT(1 - MU2*MU2)
ccc
       X1 = MAX( 1-MU1*MU1, 0.0)
       X2 = MAX( 1-MU2*MU2, 0.0)
       IF (X1 .EQ. 0.0 .OR. X2 .EQ. 0.0) THEN
          MU0 = MU1*MU2
       ELSE
          MU0 = MU1*MU2 + SQRT(X1)*SQRT(X2)*COSDAZ
       ENDIF
ccc

C      %%%normB is normalisation of mu from -1 to 1
c       NORMB = (1 - G2) * ( 1/SQRT(1 + G2 - 2*G) -
c     $    1/SQRT(1 + G2 + 2*G) ) / G
c      note: this can be re-arranged to give normb=2
C      %%%% we also know that (1/2) integral P(-1,1) = 1

       X=1 + G2 - 2*G*MU0
c       HG3 = 2 * (1 - G2) / (NORMB*X*SQRT(X))
       HG3 = (1 - G2) / (X*SQRT(X))
C
       RETURN
       END
