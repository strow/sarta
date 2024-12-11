REAL FUNCTION HG3(MU1, MU2, COSDAZ, G)
 
! Code converted using TO_F90_LOOP by Alan Miller
! Date: 2023-04-04  Time: 16:44:55

!      This is the HG (Henyey Greenstein) phase function, except it
!      uses three cosine arguments as well as asymmetry

!      Arguments

REAL, INTENT(IN)                         :: MU1
REAL, INTENT(IN)                         :: MU2
REAL, INTENT(IN)                         :: COSDAZ ! cosine of (solazi - satazi)
REAL, INTENT(IN)                         :: G


!      Local variables
REAL :: G2
REAL :: MU0
!       REAL NORMB
REAL :: X
REAL :: X1
REAL :: X2


!      ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

G2=G*G
!      mu0 = cos(theta = A - B) = cosAcosB + sinAsinB
!      mu1=cosA and mu2=cosB, and  sinX=sqrt( 1 - (cosX)^2 )
!cc
!       MU0 = MU1*MU2 + SQRT(1 - MU1*MU1)*SQRT(1 - MU2*MU2)
!cc
X1 = MAX( 1-MU1*MU1, 0.0)
X2 = MAX( 1-MU2*MU2, 0.0)
IF (X1 == 0.0 .OR. X2 == 0.0) THEN
  MU0 = MU1*MU2
ELSE
  MU0 = MU1*MU2 + SQRT(X1)*SQRT(X2)*COSDAZ
END IF
!cc

!      %%%normB is normalisation of mu from -1 to 1
!       NORMB = (1 - G2) * ( 1/SQRT(1 + G2 - 2*G) -
!     $    1/SQRT(1 + G2 + 2*G) ) / G
!      note: this can be re-arranged to give normb=2
!      %%%% we also know that (1/2) integral P(-1,1) = 1

X=1 + G2 - 2*G*MU0
!       HG3 = 2 * (1 - G2) / (NORMB*X*SQRT(X))
HG3 = (1 - G2) / (X*SQRT(X))

RETURN
END FUNCTION HG3
