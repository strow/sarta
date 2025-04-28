!=======================================================================
!    University of Maryland Baltimore County [UMBC]
!    AIRS
!    CALRAD0
!    Calculate channel radiance for a clear atmosphere above a surface
!F90====================================================================

!ROUTINE NAME:
!    CALRAD0

!ABSTRACT:
!    Calculate channel radiance for a clear atmosphere above a surface.

!CALL PROTOCOL:
!    CALRAD0(DOSUN, I, LBOT, RPLNCK, RSURFE, SECANG,
!       TAUL, TAUZ, SUNFAC, HSUN, TAUZSN, RHOSUN,
!       RHOTHR, LABOVE, COEFF, RAD0 )

!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    LOGICAL   DOSUN   do sun radiance calcs?      true/false
!    INTEGER   I       channel index               none
!    INTEGER   LBOT    bottom layer                none
!    REAL arr  RPLNCK  Planck function             mW/(m^2 cm^-1 sterad)
!    REAL      RSURFE  surface emission            mW/(m^2 cm^-1 sterad)
!    REAL arr  SECANG  path secant angles          none
!    REAL arr  TAUL    layer transmittance         none
!    REAL arr  TAUZ    surface-to-space trans      none
!    REAL      SUNFAC  sun solid angle * cosine    sterad
!    REAL arr  HSUN    solar irradiance at TOA     mW/(m^2 cm^-1 sterad)?
!    REAL arr  TAUZ    surface-to-space trans      none
!    REAL arr  TAUZSN  surface-to-space trans      none
!    REAL arr  RHOSUN  solar surface reflectivity  none
!    REAL arr  RHOTHR  down thermal surf refl      none
!    INT arr   LABOVE  down therm layer above      none
!    REAL arr  COEFF   "F" factor coefficients     various

!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL arr  RAD0    radiance                    mW/(m^2 cm^-1 sterad)

!INPUT/OUTPUT PARAMETERS:
!    none

!RETURN VALUES:
!    none

!PARENT(S):
!    sarta

!ROUTINES CALLED:
!    none

!FILES ACCESSED:
!    incFTC.f : include file of parameter statements accessed during
!       compilation only.

!COMMON BLOCKS
!    none

!DESCRIPTION:
!    Calculates the radiance for an atmosphere with no clouds.

!ALGORITHM REFERENCES:
!    none

!KNOWN BUGS AND LIMITATIONS:
!    The temperature is treated a constant within each layer (ie
!    no adjustments for temperature gradiants).

!ROUTINE HISTORY:
!    Date        Programmer     Comments
!    ----------- -------------- ----------------------------------------
!    13 Jan 2006 Scott Hannon   Created
!    29 Mar 2006 Scott Hannon   Updated RTHERM for sartaV107

!END====================================================================

!      =================================================================

SUBROUTINE CALRAD0( DOSUN, ICH, LBOT, RPLNCK, RSURFE, SECANG,  &
    TAUL, TAUZ, SUNFAC, HSUN, TAUZSN, RHOSUN, RHOTHR, LABOVE, COEFF, RAD0 )
!      =================================================================

!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
USE incFTC

!-----------------------------------------------------------------------
 IMPLICIT NONE
!-----------------------------------------------------------------------

LOGICAL, INTENT(IN)                      :: DOSUN
INTEGER, INTENT(IN)                      :: ICH
INTEGER, INTENT(IN)                      :: LBOT
REAL, INTENT(IN)                         :: RPLNCK(MAXLAY)
REAL, INTENT(IN)                         :: RSURFE
REAL, INTENT(IN)                         :: SECANG(MAXLAY)
REAL, INTENT(IN)                         :: TAUL(MAXLAY)
REAL, INTENT(IN)                         :: TAUZ(MXCHAN)
REAL, INTENT(IN)                         :: SUNFAC
REAL, INTENT(IN)                         :: HSUN(MXCHAN)
REAL, INTENT(IN)                         :: TAUZSN(MXCHAN)
REAL, INTENT(IN)                         :: RHOSUN(MXCHAN)
REAL, INTENT(IN)                         :: RHOTHR(MXCHAN)
INTEGER, INTENT(IN)                      :: LABOVE(MXCHAN)
REAL, INTENT(IN)                         :: COEFF(NFCOEF,MXCHAN)
REAL, INTENT(OUT)                        :: RAD0

!-----------------------------------------------------------------------
!      EXTERNAL FUNCTIONS
!-----------------------------------------------------------------------
!      none

!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
!      Input

!REAL :: ! layer Planck function
!REAL :: ! viewing angle secant
!REAL :: ! clear air layer transmittance
!REAL :: ! clear air surface-to-space transmittance
!      Sun info

!REAL :: ! irradiance from Sun at top of atmosphere
!REAL :: ! up plus down clear air solar transmittance
!REAL :: ! surface reflectivity for solar
!      Downwelling thermal info
!REAL :: ! surface reflectivity for downwelling thermal
!INTEGER :: ! representative layer above surface
!REAL :: ! "F" factor coefficients

!      Output

!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
INTEGER :: L      ! layer index
INTEGER :: LTHERM      ! layer for RTHERM calc
REAL :: F         ! reflected therm "F" (fudge) factor
REAL :: RADUP          ! upward radiance
REAL :: RSUN           ! reflected solar radiance
REAL :: RTHERM         ! reflected downwelling thermal radiance

!      Downwelling atmospheric thermal emission terms
REAL :: TDOWNN ! "near-side" layer-to-surface trans
REAL :: TDOWNF ! "far-side" layer-to-surface trans
REAL :: RDOWN ! downward radiance

!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none

!***********************************************************************
!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!***********************************************************************

!      -----------------------------------------------------------------
!      Loop upward over layers
!      -----------------------------------------------------------------
RADUP=RSURFE
RDOWN=0.0
TDOWNN=1.0
DO L=LBOT,1,-1
  RADUP=RADUP*TAUL(L) + RPLNCK(L)*(1.0 - TAUL(L))
  
!         Calc the downward radiance from this layer
  TDOWNF=TDOWNN*TAUL(L)
  RDOWN = RDOWN + ( RPLNCK(L)*(TDOWNN - TDOWNF) )
  TDOWNN=TDOWNF
  
  ENDDO
    
!       IF (ICH .EQ. 1291) THEN
!         PRINT *,'FINAL',RSURFE,L,RPLNCK(L),RADUP
!         PRINT *,'sergio stop A'
!       END IF
    
!      ------------------------
!      Reflected solar radiance
!      ------------------------
    IF (DOSUN) THEN
      RSUN=RHOSUN(ICH)*SUNFAC*HSUN(ICH)*TAUZSN(ICH)
    ELSE
      RSUN=0.0
    END IF
    
    
!      --------------------------------------
!      Reflected downwelling thermal radiance
!      --------------------------------------
    F=1.0
    IF (TAUZ(ICH) > 0.0005) THEN
      F=   COEFF(1,ICH) +  &
          ( COEFF(2,ICH)/SECANG(LBOT) ) +  &
          ( COEFF(3,ICH)*TAUZ(ICH) ) + & 
          ( COEFF(4,ICH)*TAUZ(ICH)*TAUZ(ICH) ) +  &
          ( COEFF(5,ICH)*TAUZ(ICH)/SECANG(LBOT) ) + &
          ( COEFF(6,ICH)*TAUZ(ICH)/RDOWN )
!         Truncate F at limits as needed
      F = MAX( MIN(F,2.09), 0.696 )
    END IF
    RTHERM=RHOTHR(ICH)*PI*RDOWN*F*TAUZ(ICH)
    
    
!      --------------
!      Total radiance
!      --------------
    RAD0=RADUP + RSUN + RTHERM
    
    
    RETURN
  END SUBROUTINE CALRAD0
