C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore County [UMBC]
C
C    AIRS
C
C    CALRAD0
C    Calculate channel radiance for a clear atmosphere above a surface
C
!F77====================================================================


!ROUTINE NAME:
C    CALRAD0


!ABSTRACT:
C    Calculate channel radiance for a clear atmosphere above a surface.


!CALL PROTOCOL:
C    CALRAD0(DOSUN, I, LBOT, RPLNCK, RSURFE, SECANG,
C       TAUL, TAUZ, SUNFAC, HSUN, TAUZSN, RHOSUN,
C       RHOTHR, LABOVE, COEFF, RAD0 )


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    LOGICAL   DOSUN   do sun radiance calcs?      true/false
C    INTEGER   I       channel index               none
C    INTEGER   LBOT    bottom layer                none
C    REAL arr  RPLNCK  Planck function             mW/(m^2 cm^-1 sterad)
C    REAL      RSURFE  surface emission            mW/(m^2 cm^-1 sterad)
C    REAL arr  SECANG  path secant angles          none
C    REAL arr  TAUL    layer transmittance         none
C    REAL arr  TAUZ    surface-to-space trans      none
C    REAL      SUNFAC  sun solid angle * cosine    sterad
C    REAL arr  HSUN    solar irradiance at TOA     mW/(m^2 cm^-1 sterad)?
C    REAL arr  TAUZ    surface-to-space trans      none
C    REAL arr  TAUZSN  surface-to-space trans      none
C    REAL arr  RHOSUN  solar surface reflectivity  none
C    REAL arr  RHOTHR  down thermal surf refl      none
C    INT arr   LABOVE  down therm layer above      none
C    REAL arr  COEFF   "F" factor coefficients     various


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    REAL arr  RAD0    radiance                    mW/(m^2 cm^-1 sterad)


!INPUT/OUTPUT PARAMETERS:
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    sarta


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    incFTC.f : include file of parameter statements accessed during
C       compilation only.


!COMMON BLOCKS
C    none


!DESCRIPTION:
C    Calculates the radiance for an atmosphere with no clouds.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    The temperature is treated a constant within each layer (ie
C    no adjustments for temperature gradiants).



!ROUTINE HISTORY:
C    Date        Programmer     Comments
C    ----------- -------------- ----------------------------------------
C    13 Jan 2006 Scott Hannon   Created
C    29 Mar 2006 Scott Hannon   Updated RTHERM for sartaV107


!END====================================================================

C      =================================================================
       SUBROUTINE CALRAD0( DOSUN, I, LBOT, RPLNCK, RSURFE, SECANG,
     $    TAUL, TAUZ, SUNFAC, HSUN, TAUZSN, RHOSUN,
     $    RHOTHR, LABOVE, COEFF, RAD0 )
C      =================================================================

C-----------------------------------------------------------------------
C      IMPLICIT NONE
C-----------------------------------------------------------------------
       IMPLICIT NONE

C-----------------------------------------------------------------------
C      INCLUDE FILES
C-----------------------------------------------------------------------
       include 'incFTC.f'

C-----------------------------------------------------------------------
C      EXTERNAL FUNCTIONS
C-----------------------------------------------------------------------
C      none

C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      Input
       LOGICAL  DOSUN      ! do sun radiance calcs?
       INTEGER      I      ! channel index
       INTEGER   LBOT      ! bottom layer of atmosphere
       REAL RPLNCK(MAXLAY) ! layer Planck function
       REAL RSURFE         ! surface emission
       REAL SECANG(MAXLAY) ! viewing angle secant
       REAL   TAUL(MAXLAY) ! clear air layer transmittance
       REAL   TAUZ(MXCHAN) ! clear air surface-to-space transmittance
C      Sun info
       REAL SUNFAC         ! sun solid angle times cosine at surface
       REAL   HSUN(MXCHAN) ! irradiance from Sun at top of atmosphere
       REAL TAUZSN(MXCHAN) ! up plus down clear air solar transmittance
       REAL RHOSUN(MXCHAN) ! surface reflectivity for solar
C      Downwelling thermal info
       REAL RHOTHR(MXCHAN) ! surface reflectivity for downwelling thermal
       INTEGER LABOVE(MXCHAN) ! representative layer above surface
       REAL  COEFF(NFCOEF,MXCHAN) ! "F" factor coefficients
C
C      Output
       REAL   RAD0                ! upwelling radiance at satellite

C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER      L      ! layer index
       INTEGER LTHERM      ! layer for RTHERM calc
       REAL      F         ! reflected therm "F" (fudge) factor
       REAL RADUP          ! upward radiance
       REAL RSUN           ! reflected solar radiance
       REAL RTHERM         ! reflected downwelling thermal radiance

C      Downwelling atmospheric thermal emission terms
       REAL TDOWNN ! "near-side" layer-to-surface trans
       REAL TDOWNF ! "far-side" layer-to-surface trans
       REAL  RDOWN ! downward radiance

C-----------------------------------------------------------------------
C      SAVE STATEMENTS
C-----------------------------------------------------------------------
C      none


C***********************************************************************
C***********************************************************************
C                    EXECUTABLE CODE
C***********************************************************************
C***********************************************************************

C      -----------------------------------------------------------------
C      Loop upward over layers
C      -----------------------------------------------------------------
       RADUP=RSURFE
       RDOWN=0.0
       TDOWNN=1.0
       DO L=LBOT,1,-1
          RADUP=RADUP*TAUL(L) + RPLNCK(L)*(1.0 - TAUL(L))

C         Calc the downward radiance from this layer
          TDOWNF=TDOWNN*TAUL(L)
          RDOWN = RDOWN + ( RPLNCK(L)*(TDOWNN - TDOWNF) )
          TDOWNN=TDOWNF

       ENDDO
C
c       IF (I .EQ. 1291) THEN
c         PRINT *,'FINAL',RSURFE,L,RPLNCK(L),RADUP
c         PRINT *,'sergio stop A'
c       END IF

C      ------------------------
C      Reflected solar radiance
C      ------------------------
       IF (DOSUN) THEN
          RSUN=RHOSUN(I)*SUNFAC*HSUN(I)*TAUZSN(I)
       ELSE
          RSUN=0.0
       ENDIF
C

C      --------------------------------------
C      Reflected downwelling thermal radiance
C      --------------------------------------
       F=1.0
       IF (TAUZ(I) .GT. 0.0005) THEN
          F=   COEFF(1,I) +
     $       ( COEFF(2,I)/SECANG(LBOT) ) +
     $       ( COEFF(3,I)*TAUZ(I) ) +
     $       ( COEFF(4,I)*TAUZ(I)*TAUZ(I) ) +
     $       ( COEFF(5,I)*TAUZ(I)/SECANG(LBOT) ) +
     $       ( COEFF(6,I)*TAUZ(I)/RDOWN )
C         Truncate F at limits as needed
          F = MAX( MIN(F,2.09), 0.696 )
       ENDIF
       RTHERM=RHOTHR(I)*PI*RDOWN*F*TAUZ(I)
C

C      --------------
C      Total radiance
C      --------------
       RAD0=RADUP + RSUN + RTHERM
C
C
       RETURN
       END
