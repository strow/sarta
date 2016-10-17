C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore County [UMBC]
C
C    AIRS
C
C    CALRAD
C
!F77====================================================================


!ROUTINE NAME:
C    CALRAD


!ABSTRACT:
C    Calculate a profile's radiance.


!CALL PROTOCOL:
C    CALRAD ( NCHAN, FREQ, TAU, TP, TBOT, EBOT, LBOT,
C             SUNCOS, RHOSUN, DISTES, HSUN, TAUZSN,
C             SEC, RHOTHR, LABOVE, COEFF, TAUZ, RAD, BT)


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    REAL arr  COEFF   thermal F factor coefs      various
C    REAL      DISTES  Earth-sun distance          meters
C    REAL arr  EBOT    bottom surface emissivity   none
C    REAL arr  FREQ    channel frequencies         cm^-1
C    INT arr   LABOVE  layer-above for thermal     none
C    INTEGER   LBOT    bottom layer                none
C    INTEGER   NCHAN   number of channels          none
C    REAL arr  RHOSUN  reflectivity for solar      1/steradian
C    REAL arr  RHOTHR  reflectivity for thermal    1/steradian
C    REAL      SEC     bottlom path angle secant   none
C    REAL      SUNCOS  sun angle cosine            none
C    REAL arr  TAU     effective layer trans       none
C    REAL arr  TAUZ    layer-to-space trans        none
C    REAL arr  TAUZSN  eff sun angle l-to-s trans  none
C    REAL      TBOT    bottom surface temperature  Kelvin
C    REAL arr  TP      temperature profile         Kelvin


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    REAL arr  BT      brightness temperature      Kelvin
C    REAL arr  RAD     radiance                    W/(m^2.str.cm^-1)


!INPUT/OUTPUT PARAMETERS:
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    USEFAST


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    incFTC.f : include file of parameter statements accessed during
C       compilation only.


!COMMON BLOCKS
C    none


!DESCRIPTION:
C    March 1998 version of the 100 layer AIRS Fast Transmittance
C    Code by L.L.Strow/S.Hannon.
C
C    The radiance is calculated for each channels in turn.  The rad
C    is a sum of four components: surface, upwelling (non-reflected)
C    atmospheric, reflected downwelling atmospheric thermal, and
C    reflected solar.  No scattering.
C
C    Comment: this routine could easily be re-written to use layer-to-
C    space transmittances rather than layer transittances.  Currently
C    the CALT# routines compute layer transmittances (since it's a bit
C    faster and more accurate when using the QIKEXP function.
C
C    ===================================================================
C    Computes black body emissions for each layer using the Planck
C    equation:
C       planck = c1*v^3/( exp(c2*v/T) - 1 )
C    where c1 and c2 are the radiation constants, T is the temperature
C    TP, and v is the frequency FREQ.
C
C    We assume the layers emit radiances of
C       rad_layer = (1 - tau)*planck
C    where tau is the layer transmittance TAU.
C
C    The total radiance leaving the bottom surface and going upward
C    is the surface emission and reflected solar & thermal. The
C    reflected solar and thermal are handled as seperate terms added
C    to radiance arriving at the satellite.
C       rad_surface =  e*planck
C    where e is the bottom surface emissivity EBOT, and the surface is
C    at temperature TBOT.
C
C    We trace the upward radiance thru the atmosphere and determine
C    the total radiance leaving the top layer (and then reaching the
C    satellite) is:
C       the sum L=L_bot downto 1 of { rad(L-1)*tau(L) + rad(L) }
C    where rad(L_bot-1) = rad_surface, and rad(1) = RAD.
C
C    The reflected solar term is based on an approximation suggested
C    by J.Susskind et al.  The reflected solar radiance reaching the
C    satellite is given by
C       Rsun = rho * omega * TAUZSN * Hsun
C    where omega is the solid angle of the sun as seen from Earth,
C    Hsun is the (non-reflected) solar radiance at the top of the
C    atmosphere, and TAUZSN is (surface) layer-to-space transmittance
C    of a path along an effective total angle defined as
C       secant_eff = secant_view + secant_sun
C    Note that this requires a seperate transmittance calculation
C    at the effective sun angle.  Hsun is passed to this routine (it
C    is close to planck for 5800 K), while omega is computed using
C    the distance of the Earth from the sun DISTES
C       omega = pi * ( radius_sun / distance_Earth_sun )^2
C
C    The reflected downwelling thermal is another approximation based
C    on a method suggested by Susskind et al.  It uses the viewing
C    angle layer-to-space transmittance TAUZ, the radiance of a
C    single layer somewhere above the surface, and a parameterized "F"
C    factor (which is sort of a fudge factor determined by regression).
C       Rtherm = rho * pi * F * planck * TAUZ*(1-TAUZ)
C    where the planck radiance is computed for layer L = LBOT - LABOVE.
C
C    For convenience we also output brightness temperature, which is
C    related to the radiance by inverting the planck equation:
C       BT = c2*v/ln( 1 + c1*v^3/RAD )
C    ===================================================================


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    Currently this routine does not handle scattering or clouds.


!ROUTINE HISTORY:
C    Date        Programmer     Comments
C    ----------- -------------- ----------------------------------------
C     2 Sep 1997 Scott Hannon   Created from an extensive re-write of
C                               our Feb97 CALRAD routine for Mar98 FTC.
C     07 Apr 2005 Scott Hannon  Change refl therm calc
C     09 May 2005 Scott Hannon  Add default F=1 to refl therm so that
C                                  RTHERM is always calculated
C     13 Dec 2005 Scott Hannon  Add limits check to "F"

!END====================================================================

C      =================================================================
       SUBROUTINE CALRAD ( NCHAN, FREQ, TAU, TP, TBOT, EBOT, LBOT,
     $    SUNCOS, RHOSUN, DISTES, HSUN, TAUZSN,
     $    SEC, RHOTHR, LABOVE, COEFF, TAUZ, RAD, BT)
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
       INTEGER NCHAN
       REAL   FREQ(MXCHAN)
       REAL    TAU(MAXLAY,MXCHAN)
       REAL     TP(MAXLAY)
       REAL   TBOT
       REAL   EBOT(MXCHAN)
       INTEGER LBOT
       REAL SUNCOS
       REAL RHOSUN(MXCHAN)
       REAL DISTES
       REAL   HSUN(MXCHAN)
       REAL TAUZSN(MXCHAN)
       REAL    SEC
       REAL RHOTHR(MXCHAN)
       INTEGER LABOVE(MXCHAN)
       REAL  COEFF(NFCOEF,MXCHAN)
       REAL   TAUZ(MXCHAN)
       REAL    RAD(MXCHAN)
       REAL     BT(MXCHAN)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER      I
       INTEGER      L
       INTEGER LTHERM
       REAL RPLNCK(MAXLAY)
       REAL   C1V3
       REAL    C2V
       REAL SUNFAC
       REAL   RSUN
       REAL      F
       REAL RTHERM

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
C
C      Note: on average, DISTES = 1.496E+11 m.  The exact value varies
C      with time since the Earth's orbit is slightly elliptical.
       SUNFAC=SUNCOS*PI*(RADSUN/DISTES)**2
C      Note: PI*(RADSUN/DISTES)^2 = omega = solid angle [steradians] of
C      the sun as seen from Earth.  The above equation is actually an
C      approximation for the case DISTES >> RADSUN.  The exact equation
C      is omega = 2*pi*(1 - DISTES/sqrt(DISTES^2 + RADSUN^2)).
C
C      ---------------------------
C      Loop on channel (frequency)
C      ---------------------------
       DO 210 I=1,NCHAN
C
C         Calc c1*v^3 and c2*v
          C1V3=C1*(FREQ(I)**3)
          C2V=C2*FREQ(I)
C          
C         ---------------------------
C         Calc the up going radiance
C         (excluding reflected terms)
C         ---------------------------
C         Initialize the upward radiance with
C         the bottom surface emission
          RAD(I)=EBOT(I)*C1V3/( EXP( C2V/TBOT ) - 1.0 )
C
C         Initialize downwelling thermal terms
          RDOWN=0.0
          TDOWNN=1.0
C
C         Loop upward over the layers
          DO L=LBOT,1,-1
C            Calculate the Planck function for this layer
             RPLNCK(L)=C1V3/( EXP( C2V/TP(L) ) - 1.0 )
C
C            Calc the upward radiance thru and from this layer
             RAD(I)=( RAD(I)*TAU(L,I) ) +
     $          ( RPLNCK(L)*(1.0E+0 - TAU(L,I)) )

C            Calc the downward radiance from this layer
             TDOWNF=TDOWNN*TAU(L,I)
             RDOWN = RDOWN + ( RPLNCK(L)*(TDOWNN - TDOWNF) )
             TDOWNN=TDOWNF

ccc
c      if (I .EQ. 1928) then
c      print*, RAD(I)
c      endif
ccc


          ENDDO
C
C         --------------------------
C         Calc the reflected solar
C         rad reaching the satellite
C         --------------------------
          RSUN=RHOSUN(I)*SUNFAC*HSUN(I)*TAUZSN(I)
C
C         ----------------------------------
C         Calc the reflected downwelling
C         thermal rad reaching the satellite
C         ----------------------------------
          F=1.0
          IF (TAUZ(I) .GT. 0.0005) THEN
             F=   COEFF(1,I) +
     $          ( COEFF(2,I)/SEC ) +
     $          ( COEFF(3,I)*TAUZ(I) ) +
     $          ( COEFF(4,I)*TAUZ(I)*TAUZ(I) ) +
     $          ( COEFF(5,I)*TAUZ(I)/SEC ) +
     $          ( COEFF(6,I)*TAUZ(I)/RDOWN )
C            Truncate F at limits as needed
             F = MAX( MIN(F,2.09), 0.696 )
          ENDIF
          RTHERM=RHOTHR(I)*PI*RDOWN*F*TAUZ(I)
C
C         --------------------------------------------------
C         Add on the reflected solar and downwelling thermal
C         --------------------------------------------------
ccc
c for testing
c      RTHERM=0.0
c      RSUN=0.0
ccc
          RAD(I)=RAD(I) + RSUN + RTHERM
C
C         ------------------------------------------
C         Convert radiance to brightness temperature
C         ------------------------------------------
ccc removed 07 Apr 2005 since never used
c          BT(I)=C2V/LOG( 1.0 + C1V3/RAD(I) )
ccc
C
 210   CONTINUE
C      End loops on channel number (frequency)
C
       RETURN
       END
