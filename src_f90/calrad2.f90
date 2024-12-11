!=======================================================================
 
! Code converted using TO_F90_LOOP by Alan Miller
! Date: 2023-04-04  Time: 16:44:55
 
!=======================================================================

!    University of Maryland Baltimore County [UMBC]

!    AIRS

!    CALRAD2
!    Calculate the channel radiance for an atmosphere with two clouds.
!    This version of CALRAD uses "Parameterization for Cloud
!    Longwave Scattering for Atmospheric Models" (PCLSAM)

!F77====================================================================


!ROUTINE NAME:
!    CALRAD2


!ABSTRACT:
!    Calculate the channel radiance for an atmosphere with two clouds.


!CALL PROTOCOL:
!    CALRAD2( DOSUN, I, LBOT, RPLNCK, RSURFE, SECANG,
!       ODL, TAUL, TAUZ, SUNFAC, HSUN, TAUZSN, RHOSUN,
!       RHOTHR, LABOVE, COEFF,
!       CFRCL1, MASEC1, MASUN1, NEXTO1, NSCAO1, G_ASY1, LCTOP1, LCBOT1,
!       CFRCL2, MASEC2, MASUN2, COSDAZ,
!       NEXTO2, NSCAO2, G_ASY2, LCTOP2, LCBOT2, RAD2 )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    LOGICAL   DOSUN   do sun radiance calcs?      true/false
!    INTEGER   I       channel index               none
!    INTEGER   LBOT    bottom layer                none
!    REAL arr  RPLNCK  Planck function             mW/(m^2 cm^-1 sterad)
!    REAL arr  RSURFE  surface emission            mW/(m^2 cm^-1 sterad)
!    REAL arr  SECANG  path secant angles          none
!    REAL arr  TAU     layer transmittances        none
!    REAL arr  TAUZ    surface-to-space trans      none
!    REAL      SUNFAC  sun solid angle * cosine    sterad
!    REAL arr  HSUN    solar irradiance at TOA     mW/(m^2 cm^-1 sterad)?
!    REAL arr  TAUZ    surface-to-space trans      none
!    REAL arr  TAUZSN  surface-to-space trans      none
!    REAL arr  RHOSUN  solar surface reflectivity  none
!    REAL arr  RHOTHR  down thermal surf refl      none
!    INT arr   LABOVE  down therm layer above      none
!    REAL arr  COEFF   "F" factor coefficients     various
!    REAL arr  CFRCL1  fraction of cloud in layer  none
!    REAL      MASEC1  mean view secant in cloud   none
!    REAL      MASUN1  mean sun-only sec in cloud  none
!    REAL arr  NEXTO1  cloud extinction opt depth  none
!    REAL arr  NSCAO1  cloud scattering opt depth  none
!    REAL arr  G_ASY1  cloud asymmetry parameter   none
!    INTEGER   LCTOP1  cloud top layer index       none
!    INTEGER   LCBOT1  cloud bottom layer index    none
!    REAL arr  CFRCL2  fraction of cloud in layer  none
!    REAL      MASEC2  mean view secant in cloud   none
!    REAL      MASUN2  mean sun-only sec in cloud  none
!    REAL      COSDAZ  cosine(satazi - solazi)     none
!    REAL arr  NEXTO2  cloud extinction opt depth  none
!    REAL arr  NSCAO2  cloud scattering opt depth  none
!    REAL arr  G_ASY2  cloud asymmetry parameter   none
!    INTEGER   LCTOP2  cloud top layer index       none
!    INTEGER   LCBOT2  cloud bottom layer index    none

!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL arr  RAD2    radiance                    mW/(m^2 cm^-1 sterad)


!INPUT/OUTPUT PARAMETERS:
!    none


!RETURN VALUES:
!    none


!PARENT(S):
!    sarta


!ROUTINES CALLED:
!    function QIKEXP = EXP(X) calculation, faster than EXP(X) if X is small.
!    function HG3 = HG phase function


!FILES ACCESSED:
!    incFTC.f : include file of parameter statements accessed during
!       compilation only.


!COMMON BLOCKS
!    none


!DESCRIPTION:
!    Calculates the channel radiance for an atmosphere with two clouds
!    with the PCLSAM method.


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    The temperature is treated a constant within each layer (ie
!    no adjustments for temperature gradiants).

!    The reflected downwelling emission term is adjusted for the
!    clouds after it is reflected from the surface, but the downward
!    radiance hitting the surface is for a clear sky.



!ROUTINE HISTORY:
! Date        Programmer     Comments
! ----------- -------------- -------------------------------------------
! 13 Jan 2006 Scott Hannon   Created
! 01 Feb 2006 S.Machado      correct solar scattering by 2pi (replace
!    0.5 by PI4INV)
! 03 Feb 2006 S.Hannon       Remove scattering adjustments from some
!    downward optical depths to be consistenct with kcarta. Correct
!    ODSUM=0 initialization by moving it outside layer loop.
! 08 Feb 2006 S.Hannon       Correct error in RSUNSC so HSUN is
!    multiplied by SUNFAC not SCOSL
! 28 Mar 2006 S.Hannon       Implement Sergio's RSUNSC fudge
!    (non-standard PCLSAM) to improve agreement with other codes.
!    Change ODTOTZ used by RSUNSC to layer-above-to-space (was
!    (layer-to-space).
! 29 Mar 2006 S.Hannon       Updated RTHERM for sartaV107
! 03 Apr 2006 S.Hannon/S.Machado  Add missing w_tilde to RSUNSC
! 24 Mar 2008 S.Hannon       Add COSDAZ and use HG3 instead of HG2
! 29 Apr 2009 S.Hannon       Bug fix: initialize RDOWN and TDOWNN


!END====================================================================

!      =================================================================

SUBROUTINE CALRAD2( DOSUN, I, LBOT, RPLNCK, RSURFE, SECANG,  &
    ODL, TAUL, TAUZ, SUNFAC, HSUN, TAUZSN, RHOSUN,  &
    RHOTHR, LABOVE, COEFF, CFRCL1, MASEC1, MASUN1,  &
    NEXTO1, NSCAO1, G_ASY1, LCTOP1,LCBOT1, CFRCL2, MASEC2, MASUN2, COSDAZ,  &
    NEXTO2, NSCAO2, G_ASY2, LCTOP2,LCBOT2, RAD2 )
!      =================================================================

!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------

LOGICAL, INTENT(IN)                      :: DOSUN
INTEGER, INTENT(IN OUT)                  :: I
INTEGER, INTENT(IN)                      :: LBOT
REAL, INTENT(IN)                         :: RPLNCK(MAXLAY)
REAL, INTENT(IN)                         :: RSURFE
REAL, INTENT(IN)                         :: SECANG(MAXLAY)
REAL, INTENT(IN)                         :: ODL(MAXLAY,MXCHAN)
REAL, INTENT(IN)                         :: TAUL(MAXLAY)
REAL, INTENT(IN)                         :: TAUZ(MXCHAN)
REAL, INTENT(IN)                         :: SUNFAC
REAL, INTENT(IN)                         :: HSUN(MXCHAN)
REAL, INTENT(IN)                         :: TAUZSN(MXCHAN)
REAL, INTENT(IN)                         :: RHOSUN(MXCHAN)
REAL, INTENT(IN)                         :: RHOTHR(MXCHAN)
INTEGER, INTENT(IN OUT)                  :: LABOVE(MXCHAN)
REAL, INTENT(IN)                         :: COEFF(NFCOEF,MXCHAN)
REAL, INTENT(IN)                         :: CFRCL1(MAXLAY)
REAL, INTENT(IN)                         :: MASEC1
REAL, INTENT(IN)                         :: MASUN1
REAL, INTENT(IN)                         :: NEXTO1(MXCHAN)
REAL, INTENT(IN)                         :: NSCAO1(MXCHAN)
REAL, INTENT(IN)                         :: G_ASY1(MXCHAN)
INTEGER, INTENT(IN OUT)                  :: LCTOP1
INTEGER, INTENT(IN OUT)                  :: LCBOT1
REAL, INTENT(IN)                         :: CFRCL2(MAXLAY)
REAL, INTENT(IN OUT)                     :: MASEC2
REAL, INTENT(IN)                         :: MASUN2
REAL, INTENT(IN OUT)                     :: COSDAZ
REAL, INTENT(IN)                         :: NEXTO2(MXCHAN)
REAL, INTENT(IN)                         :: NSCAO2(MXCHAN)
REAL, INTENT(IN)                         :: G_ASY2(MXCHAN)
INTEGER, INTENT(IN OUT)                  :: LCTOP2
INTEGER, INTENT(IN OUT)                  :: LCBOT2
REAL, INTENT(OUT)                        :: RAD2
IMPLICIT NONE

!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
INCLUDE 'incFTC.f'

!-----------------------------------------------------------------------
!      EXTERNAL FUNCTIONS
!-----------------------------------------------------------------------
!      QIKEXP  : see file 'qikexp.f'
!      HG3     : see file 'hg3.f'

!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
!      Input



REAL :: ! layer Planck function

REAL :: ! viewing angle secant
REAL :: ! clear air layer optical depth
REAL :: ! clear air layer transmittance
REAL :: ! clear air surface-to-space transmittance
!      Sun info

REAL :: ! irradiance from Sun at top of atmosphere
REAL :: ! up plus down clear air solar transmittance
REAL :: ! surface reflectivity for solar
!      Downwelling thermal info
REAL :: ! surface reflectivity for downwelling thermal
INTEGER :: ! representative layer above surface
REAL :: ! "F" factor coefficients
!      Cloud1 info
REAL :: ! fraction of cloud1 in layer


REAL :: ! cloud1 nadir extinction optical depth
REAL :: ! cloud1 nadir scattering optical depth
REAL :: ! cloud1 asymmetry


!      Cloud2 info
REAL :: ! fraction of cloud2 in layer



REAL :: ! cloud2 nadir extinction optical depth
REAL :: ! cloud2 nadir scattering optical depth
REAL :: ! cloud2 asymmetry



!      Output


!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
LOGICAL :: DOSUNL(MAXLAY) ! layer solar scattering true/false
LOGICAL :: LCLOUD      ! layer contains cloud true/false
INTEGER :: L      ! layer index
INTEGER :: LTHERM      ! layer for RTHERM calc
REAL :: F         ! reflected therm "F" (fudge) factor
REAL :: GL(MAXLAY) ! layer scattering asymmetry
REAL :: K1         ! cloud1 optical depth
REAL :: K1L        ! cloud1 optical depth in current layer
REAL :: K2         ! cloud2 optical depth
REAL :: K2L        ! cloud2 optical depth in current layer
REAL :: KAIR        ! air (no cloud) optical depth in current layer
REAL :: ODSUM         ! sum of optical depth
REAL :: ODTOTL(MAXLAY) ! total nadir layer optical depth
REAL :: ODTOTZ(MAXLAY) ! total nadir layer-to-space optical depth
REAL :: RADUP          ! upward radiance
REAL :: RSUN           ! reflected solar radiance
REAL :: RSUNSC         ! scatter solar radiance
REAL :: RTHERM         ! reflected downwelling thermal radiance
REAL :: SSECL(MAXLAY) ! solar angle secant
REAL :: SCOSL(MAXLAY) ! solar angle cosine
REAL :: TAULX(MAXLAY) ! layer transmittance
REAL :: TAUZCU         ! cloud transmittance at upward view angle
REAL :: TAUZCD         ! cloud transmittance at downward sun angle
REAL :: VCOSL(MAXLAY) ! view angle cosine
REAL :: PI4INV         ! 1/4pi

REAL :: XFUDGE(MAXLAY) ! Sergio's fudged optical depth for RSUNSC
REAL :: WTILDE(MAXLAY) ! single scattering albedo including KAIR

!      Downwelling atmospheric thermal emission terms
REAL :: TDOWNN ! "near-side" layer-to-surface trans
REAL :: TDOWNF ! "far-side" layer-to-surface trans
REAL :: RDOWN ! downward radiance

!      for function QIKEXP
REAL :: QIKEXP

!      for function HG3
REAL :: HG3


!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none


!***********************************************************************
!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!***********************************************************************

PI4INV = 1.0/(4.0*PI)

!      Cloud optical depths adjusted for scattering
K1=NEXTO1(I) - NSCAO1(I)*(1.0+G_ASY1(I))/2.0
K2=NEXTO2(I) - NSCAO2(I)*(1.0+G_ASY2(I))/2.0

!      -----------------------------------------------------------------
!      Loop downward over the layers
!      -----------------------------------------------------------------
ODSUM=0.0
DO L=1,LBOT
  DOSUNL(L)=.FALSE.
  LCLOUD=.FALSE.
  
  KAIR=ODL(L,I)/SECANG(L)
! added 28 Mar 2006; layer-above-to-space
  ODTOTZ(L)=ODSUM
  WTILDE(L)=0.0
  
  IF (CFRCL1(L) > 0.0) THEN
    LCLOUD=.TRUE.
    SSECL(L)=MASUN1       ! note: if no sun, this is garbage
    SCOSL(L)=1.0/SSECL(L) ! note: if no sun, this is garbage
    VCOSL(L)=1.0/SECANG(L)
    GL(L)=G_ASY1(I)
    DOSUNL(L)=DOSUN
    K1L=CFRCL1(L)*K1
  ELSE
    K1L=0.0
  END IF
  
  IF (CFRCL2(L) > 0.0) THEN
    LCLOUD=.TRUE.
    SSECL(L)=MASUN2       ! note: if no sun, this is garbage
    SCOSL(L)=1.0/SSECL(L) ! note: if no sun, this is garbage
    VCOSL(L)=1.0/SECANG(L)
    GL(L)=G_ASY2(I)
    DOSUNL(L)=DOSUN
    K2L=CFRCL2(L)*K2
    IF (CFRCL1(L) > 0.0) THEN
!               Compute weighted mean asymmetry factor for both clouds
      GL(L)=( CFRCL1(L)*NSCAO1(I)*G_ASY1(I) +  &
          CFRCL2(L)*NSCAO2(I)*G_ASY2(I) ) /  &
          ( CFRCL1(L)*NSCAO1(I)+CFRCL2(L)*NSCAO2(I) )
    END IF
  ELSE
    K2L=0.0
  END IF
  
  IF (LCLOUD) THEN
    ODTOTL(L)=KAIR + K1L + K2L
! replaced 03Feb2006             ODSUM=ODSUM + ODTOTL(L)
    ODSUM=ODSUM +KAIR +CFRCL1(L)*NEXTO1(I) +CFRCL2(L)*NEXTO2(I)
    TAULX(L)=QIKEXP( -ODTOTL(L)*SECANG(L) )
    XFUDGE(L)=KAIR +CFRCL1(L)*NEXTO1(I) +CFRCL2(L)*NEXTO2(I)
    WTILDE(L)=( CFRCL1(L)*NSCAO1(I) + CFRCL2(L)*NSCAO2(I) ) / XFUDGE(L)
  ELSE
    XFUDGE(L)=KAIR
    ODTOTL(L)=KAIR
    ODSUM=ODSUM + KAIR
    TAULX(L)=TAUL(L)
  END IF
! removed 28 Mar 2006; layer-to-space
!          ODTOTZ(L)=ODSUM
  
  ENDDO ! downward loop over layers
    
!      Calc the surface-to-space transmittance thru the clouds(only)
    TAUZCU=QIKEXP(-K1*MASEC1 -K2*MASEC2) ! upward path
    
    
!      -----------------------------------------------------------------
!      Loop upward over layers
!      -----------------------------------------------------------------
    RADUP=RSURFE
    RDOWN=0.0
    TDOWNN=1.0
    DO L=LBOT,1,-1
      IF (DOSUNL(L)) THEN
!            Scattered solar
        RSUNSC=(SCOSL(L)/(VCOSL(L)+SCOSL(L)))*PI4INV*WTILDE(L)*  &
            HG3(-SCOSL(L),VCOSL(L),COSDAZ,GL(L))*SUNFAC*HSUN(I)*  &
            QIKEXP( -ODTOTZ(L)*SSECL(L) )*
!cc fudged PCLSAM equation uses XFUDGE instead of ODTOTL  &
        (1.0 - QIKEXP( -XFUDGE(L)*(SECANG(L)+SSECL(L)) ))
!cc standard PCLSAM equation
!     $          (1.0 - QIKEXP( -ODTOTL(L)*(SECANG(L)+SSECL(L)) ))
! comment: According to Sergio Machado, the standard PCLSAM equation
! under-estimates the amount of solar radianced scattered into the
! view angle (RSUNSC).  If ODTOTL is replaced by XFUDGE the solar
! scattering term is increased.
        
      ELSE
        RSUNSC=0.0
      END IF
      
!         Calc the downward radiance from this layer
      TDOWNF=TDOWNN*TAULX(L)
      RDOWN = RDOWN + ( RPLNCK(L)*(TDOWNN - TDOWNF) )
      TDOWNN=TDOWNF
      
      RADUP=RADUP*TAULX(L) + RPLNCK(L)*(1.0 - TAULX(L)) + RSUNSC
      ENDDO
        
        
!      ------------------------
!      Reflected solar radiance
!      ------------------------
        IF (DOSUN) THEN
! replaced 03Feb2006          TAUZCD=QIKEXP(-MASUN1*K1-MASUN2*K2) ! downward path
          TAUZCD=QIKEXP(-MASUN1*NEXTO1(I)-MASUN2*NEXTO2(I)) ! downward path
          RSUN=RHOSUN(I)*SUNFAC*HSUN(I)*TAUZSN(I)*TAUZCU*TAUZCD
        ELSE
          RSUN=0.0
        END IF
        
        
!      --------------------------------------
!      Reflected downwelling thermal radiance
!      --------------------------------------
        F=1.0
        IF (TAUZ(I) > 0.0005) THEN
          F=   COEFF(1,I) + ( COEFF(2,I)/SECANG(LBOT) ) +  &
              ( COEFF(3,I)*TAUZ(I) ) + ( COEFF(4,I)*TAUZ(I)*TAUZ(I) ) +  &
              ( COEFF(5,I)*TAUZ(I)/SECANG(LBOT) ) + ( COEFF(6,I)*TAUZ(I)/RDOWN )
!         Truncate F at limits as needed
          F = MAX( MIN(F,2.09), 0.696 )
        END IF
        RTHERM=RHOTHR(I)*PI*RDOWN*F*TAUZ(I)*TAUZCU
        
        
!      --------------
!      Total radiance
!      --------------
        RAD2=RADUP + RSUN + RTHERM
        
        RETURN
      END SUBROUTINE CALRAD2
