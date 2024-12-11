!=======================================================================
!
!    University of Maryland Baltimore County [UMBC]
!
!    AIRS
!
!    CALRAD
!
!F90====================================================================


!ROUTINE NAME:
!    CALRAD


!ABSTRACT:
!    Calculate a profile's radiance.


!CALL PROTOCOL:
!    CALRAD ( NCHAN, FREQ, TAU, TP, TBOT, EBOT, LBOT,
!             SUNCOS, RHOSUN, DISTES, HSUN, TAUZSN,
!             SEC, RHOTHR, LABOVE, COEFF, TAUZ, RAD, BT)


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL arr  COEFF   thermal F factor coefs      various
!    REAL      DISTES  Earth-sun distance          meters
!    REAL arr  EBOT    bottom surface emissivity   none
!    REAL arr  FREQ    channel frequencies         cm^-1
!    INT arr   LABOVE  layer-above for thermal     none
!    INTEGER   LBOT    bottom layer                none
!    INTEGER   NCHAN   number of channels          none
!    REAL arr  RHOSUN  reflectivity for solar      1/steradian
!    REAL arr  RHOTHR  reflectivity for thermal    1/steradian
!    REAL      SEC     bottlom path angle secant   none
!    REAL      SUNCOS  sun angle cosine            none
!    REAL arr  TAU     effective layer trans       none
!    REAL arr  TAUZ    layer-to-space trans        none
!    REAL arr  TAUZSN  eff sun angle l-to-s trans  none
!    REAL      TBOT    bottom surface temperature  Kelvin
!    REAL arr  TP      temperature profile         Kelvin
!    REAL      SECFAC  convert tau to diffu angle  none


!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL arr  BT      brightness temperature      Kelvin
!    REAL arr  RAD     radiance                    W/(m^2.str.cm^-1)


!INPUT/OUTPUT PARAMETERS:
!    none


!RETURN VALUES:
!    none


!PARENT(S):
!    USEFAST


!ROUTINES CALLED:
!    none


!FILES ACCESSED:
!    incFTC.f : include file of parameter statements accessed during
!       compilation only.


!COMMON BLOCKS
!    none


!DESCRIPTION:
!    March 1998 version of the 100 layer AIRS Fast Transmittance
!    Code by L.L.Strow/S.Hannon.
!
!    The radiance is calculated for each channels in turn.  The rad
!    is a sum of four components: surface, upwelling (non-reflected)
!    atmospheric, reflected downwelling atmospheric thermal, and
!    reflected solar.  No scattering.
!
!    Comment: this routine could easily be re-written to use layer-to-
!    space transmittances rather than layer transittances.  Currently
!    the CALT# routines compute layer transmittances (since it's a bit
!    faster and more accurate when using the QIKEXP function.
!
!    ===================================================================
!    Computes black body emissions for each layer using the Planck
!    equation:
!       planck = c1*v^3/( exp(c2*v/T) - 1 )
!    where c1 and c2 are the radiation constants, T is the temperature
!    TP, and v is the frequency FREQ.
!
!    We assume the layers emit radiances of
!       rad_layer = (1 - tau)*planck
!    where tau is the layer transmittance TAU.
!
!    The total radiance leaving the bottom surface and going upward
!    is the surface emission and reflected solar & thermal. The
!    reflected solar and thermal are handled as seperate terms added
!    to radiance arriving at the satellite.
!       rad_surface =  e*planck
!    where e is the bottom surface emissivity EBOT, and the surface is
!    at temperature TBOT.
!
!    We trace the upward radiance thru the atmosphere and determine
!    the total radiance leaving the top layer (and then reaching the
!    satellite) is:
!       the sum L=L_bot downto 1 of { rad(L-1)*tau(L) + rad(L) }
!    where rad(L_bot-1) = rad_surface, and rad(1) = RAD.
!
!    The reflected solar term is based on an approximation suggested
!    by J.Susskind et al.  The reflected solar radiance reaching the
!    satellite is given by
!       Rsun = rho * omega * TAUZSN * Hsun
!    where omega is the solid angle of the sun as seen from Earth,
!    Hsun is the (non-reflected) solar radiance at the top of the
!    atmosphere, and TAUZSN is (surface) layer-to-space transmittance
!    of a path along an effective total angle defined as
!       secant_eff = secant_view + secant_sun
!    Note that this requires a seperate transmittance calculation
!    at the effective sun angle.  Hsun is passed to this routine (it
!    is close to planck for 5800 K), while omega is computed using
!    the distance of the Earth from the sun DISTES
!       omega = pi * ( radius_sun / distance_Earth_sun )^2
!
!    The reflected downwelling thermal is another approximation based
!    on a method suggested by Susskind et al.  It uses the viewing
!    angle layer-to-space transmittance TAUZ, the radiance of a
!    single layer somewhere above the surface, and a parameterized "F"
!    factor (which is sort of a fudge factor determined by regression).
!       Rtherm = rho * pi * F * planck * TAUZ*(1-TAUZ)
!    where the planck radiance is computed for layer L = LBOT - LABOVE.
!
!    For convenience we also output brightness temperature, which is
!    related to the radiance by inverting the planck equation:
!       BT = c2*v/ln( 1 + c1*v^3/RAD )
!    ===================================================================


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    Currently this routine does not handle scattering or clouds.


!ROUTINE HISTORY:
!    Date        Programmer     Comments
!    ----------- -------------- ----------------------------------------
!     2 Sep 1997 Scott Hannon   Created from an extensive re-write of
!                               our Feb97 CALRAD routine for Mar98 FTC.
!     07 Apr 2005 Scott Hannon  Change refl therm calc
!     09 May 2005 Scott Hannon  Add default F=1 to refl therm so that
!                                  RTHERM is always calculated
!     13 Dec 2005 Scott Hannon  Add limits check to "F"

!END====================================================================

!      =================================================================
       SUBROUTINE CALRAD ( NCHAN, FREQ, TAU, TP, TBOT, EBOT, LBOT, &
        SUNCOS, RHOSUN, HSUN, TAUZSN, &
        SEC, RHOTHR, LABOVE, COEFF, TAUZ, RAD, BT)
!      =================================================================

!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
use incFTC

!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------
       IMPLICIT NONE

!-----------------------------------------------------------------------
!      EXTERNAL FUNCTIONS
!-----------------------------------------------------------------------
!      none

!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
!       INTEGER NCHAN
!       REAL   FREQ(MXCHAN)
!       REAL    TAU(MAXLAY,MXCHAN)
!       REAL     TP(MAXLAY)
!       REAL   TBOT
!       REAL   EBOT(MXCHAN)
!       INTEGER LBOT
!       REAL SUNCOS
!       REAL RHOSUN(MXCHAN)
!       REAL DISTES
!       REAL   HSUN(MXCHAN)
!       REAL TAUZSN(MXCHAN)
!       REAL    SEC
!       REAL RHOTHR(MXCHAN)
!       INTEGER LABOVE(MXCHAN)
!       REAL  COEFF(NFCOEF,MXCHAN)
!       REAL   TAUZ(MXCHAN)
!       REAL    RAD(MXCHAN)
!       REAL     BT(MXCHAN)
 real(4), dimension(MXCHAN) :: FREQ, EBOT, RHOSUN, HSUN, TAUZSN, RHOTHR
 real(4), dimension(MXCHAN) :: TAUZ, RAD, BT
 real(4), dimension(MAXLAY) :: TP
 real(4), dimension(MAXLAY,MXCHAN) :: TAU
 real(4), dimension(NFCOEF,MXCHAN) :: COEFF
 real(4) :: TBOT, SUNCOS, SEC
 integer :: NCHAN, LBOT
 integer, dimension(MXCHAN) :: LABOVE

!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
integer ::  I, L, LTHERM
real(4),dimension(MAXLAY) ::  RPLNCK
real(4) ::  C1V3, C2V, SUNFAC, RSUN,  F,  RTHERM

!      Downwelling atmospheric thermal emission terms
!  TDOWNN ! "near-side" layer-to-surface trans
!  TDOWNF ! "far-side" layer-to-surface trans
!  RDOWN ! downward radiance
!  SECFAC ! Convert tau from obs angle to diffsivity angle 
real(4) :: TDOWNN, TDOWNF, RDOWN, SECFAC

!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none

!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!
!      Note: on average, DISTES = 1.496E+11 m.  The exact value varies
!      with time since the Earth's orbit is slightly elliptical.
       SUNFAC=SUNCOS*PI*(RADSUN/DISTES)**2
!      Note: PI*(RADSUN/DISTES)^2 = omega = solid angle [steradians] of
!      the sun as seen from Earth.  The above equation is actually an
!      approximation for the case DISTES >> RADSUN.  The exact equation
!      is omega = 2*pi*(1 - DISTES/sqrt(DISTES^2 + RADSUN^2)).
!
!      ---------------------------
!      Loop on channel (frequency)
!      ---------------------------
       SECFAC = 1.66/SEC
       DO 210 I=1,NCHAN
!
!         Calc c1*v^3 and c2*v
          C1V3=C1*(FREQ(I)**3)
          C2V=C2*FREQ(I)
!          
!         ---------------------------
!         Calc the up going radiance
!         (excluding reflected terms)
!         ---------------------------
!         Initialize the upward radiance with
!         the bottom surface emission
          RAD(I)=EBOT(I)*C1V3/( EXP( C2V/TBOT ) - 1.0 )
!  if(I .eq. 99) then
!    write(6,*) 'calrad:rad(99).init ', RAD(I)
!  endif
!
!         Initialize downwelling thermal terms
          RDOWN=0.0
          TDOWNN=1.0
!
!         Loop upward over the layers
          DO L=LBOT,1,-1
!            Calculate the Planck function for this layer
             RPLNCK(L)=C1V3/( EXP( C2V/TP(L) ) - 1.0 )
!
!            Calc the upward radiance thru and from this layer
             RAD(I)=( RAD(I)*TAU(L,I) ) + ( RPLNCK(L)*(1.0E+0 - TAU(L,I)) )

!            Calc the downward radiance from this layer
             TDOWNF=TDOWNN*TAU(L,I)
!$$$             TDOWNF=TDOWNN*TAU(L,I)**SECFAC
             RDOWN = RDOWN + ( RPLNCK(L)*(TDOWNN - TDOWNF) )
             TDOWNN=TDOWNF

!cc
!      if (I .EQ. 1928) then
!      print*, TAU(L,I)
!      endif
!cc
          ENDDO
!
!         --------------------------
!         Calc the reflected solar
!         rad reaching the satellite
!         --------------------------
          RSUN=RHOSUN(I)*SUNFAC*HSUN(I)*TAUZSN(I)
!
!         ----------------------------------
!         Calc the reflected downwelling
!         thermal rad reaching the satellite
!         ----------------------------------
          F=1.0
          IF (TAUZ(I) .GT. 0.0005) THEN
             F=   COEFF(1,I) + &
                ( COEFF(2,I)/SEC ) + &
                ( COEFF(3,I)*TAUZ(I) ) + &
                ( COEFF(4,I)*TAUZ(I)*TAUZ(I) ) + &
                ( COEFF(5,I)*TAUZ(I)/SEC ) + &
                ( COEFF(6,I)*TAUZ(I)/RDOWN )
!            Truncate F at limits as needed
             F = MAX( MIN(F,2.09), 0.696 )
          ENDIF
          RTHERM=RHOTHR(I)*PI*RDOWN*F*TAUZ(I)
!$$$          RTHERM=RHOTHR(I)*PI*RDOWN*TAUZ(I)
!
!         --------------------------------------------------
!         Add on the reflected solar and downwelling thermal
!         --------------------------------------------------
!cc
! for testing
!      RTHERM=0.0
!      RSUN=0.0
!cc
!  if(I .eq. 99) then
!    write(6,*) 'calrad: rad(99),rsun,rtherm',RAD(I),RSUN,RTHERM
!  endif 
          RAD(I)=RAD(I) + RSUN + RTHERM
!
!         ------------------------------------------
!         Convert radiance to brightness temperature
!         ------------------------------------------
!cc removed 07 Apr 2005 since never used
          BT(I)=C2V/LOG( 1.0 + C1V3/RAD(I) )
!cc
!
 210   CONTINUE
!      End loops on channel number (frequency)
!
       RETURN
       END
