c this changes radiances to BTs

      SUBROUTINE  RAD2BT(FREQ, RAD, BT, NCHAN);

C-----------------------------------------------------------------------
C      IMPLICIT NONE
C-----------------------------------------------------------------------
       IMPLICIT NONE

C-----------------------------------------------------------------------
C      INCLUDE FILES
C-----------------------------------------------------------------------
       include 'incFTC.f'
       include 'rtpdefs.f'

c input
      INTEGER NCHAN
      REAL FREQ(MXCHAN)
      REAL RAD(MXCHAN)

c output
      REAL BT(MXCHAN)

c local
c      REAL C, H, K, C1, C2
      INTEGER i
     
c Constants; values from NIST (CODATA98)
c      C = 2.99792458e+08  ! speed of light      299 792 458 m s-1
c      H = 6.62606876e-34  ! Planck constant     6.626 068 76 x 10-34 J s
c      K = 1.3806503e-23   ! Boltzmann constant  1.380 6503 x 10-23 J K-1

c Compute radiation constants c1 and c2
c      C1 = 2*H*C*C * 1e+11
c      C2 = (H*C/K) * 100

c the scalar calculation, for reference
c BT = c2 * FREQ / log(1 + c1 * FREQ^3 / rad)
      DO I = 1,NCHAN
        BT(I) = C2 * FREQ(I) / LOG(1 + C1 * (FREQ(I)**3)/(RAD(I)))
      END DO

      RETURN
      END
      
