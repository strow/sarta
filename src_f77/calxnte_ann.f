C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore County [UMBC]
C
C    AIRS
C
C    CALXNTE_ANN (for Non-local Thermodynamic Equilibrium)
C            extended solar angle range.
!F77====================================================================


!ROUTINE NAME:
C    CALXNTE_ANN


!ABSTRACT:
C    Adjust a LTE atmospheric radiance for a non-LTE upper atmosphere.


!CALL PROTOCOL:
C    CALNTE( INDCHN, TEMP, SUNCOS, SCOS1, VSEC1,
C       NCHNTE, CLISTN, COEFN, CO2TOP, RAD)


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INT arr   INDCHN  channel indices             none
C    REAL arr  TEMP    temperature profile         Kelvin
C    REAL      SZALAY  solzen at layer1            degrees
C    REAL      SUNCOS  solzen cosine at surface    none
C    REAL      SCOS1   solzen cosine at layer1     none
C    REAL      VSEC1   satzen secant at layer1     none
C    INTEGER   NCHNTE  number of non-LTE channels  none
C    INT arr   CLISTN  non-LTE channel list        none
C    REAL arr  COEFN   non-LTE coefficients        various
C    REAL arr  CO2TOP  top layers CO2 mixing ratio ppmv


!OUTPUT PARAMETERS:
C    none


!INPUT/OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    REAL arr  RAD     radiance                    W/(m^2.str.cm^-1)

!RETURN VALUES:
C    none


!PARENT(S):
C    SARTA


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    incFTC.f : include file of parameter statements accessed during
C       compilation only.


!COMMON BLOCKS
C    none

!DESCRIPTION:
C    May 2008 version of the 100 layer AIRS Fast Transmittance
C    Code by L.L.Strow/S.Hannon.
C
C    In the upper atmosphere where the air is thin, the strong CO2
C    absoption bands in the 4 um region can absorb solar radiance
C    faster than collisons with other air molecules can re-distribute
C    the energy. The CO2 is no longer in thermodynamic equilibrium
C    with its surroundings, which results in a change to the CO2
C    vibrational band population statistics and its effective
C    radiating temperature.  This code applies a regression based
C    adjustment to the input radiance to account for non-LTE effects.
C    Coefficients and predictors are multiplied together and summed
C    to calculate the change in radiance for non-LTE conditions.
C

!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C Date        Programmer     Comments
C ----------- -------------- ------------------------------------------
C 15 Mar 2005 Scott Hannon   Created
C 13 Oct 2005 Scott Hannon   MXCHNN renamed MXCNTE to avoid conflict
C                               with MXCHNN used with N2O
C 14 May 2008 Scott Hannon   Add CO2 adjustment using 7th coef; pass in
C                               CO2TOP
C    Jun 2022 C Hepplewhite  Add extended range byond solzen>90
C    twilight.
!END====================================================================

C      =================================================================
       SUBROUTINE CALNTE ( INDCHN, TEMP, VSEC1,
     $    NCHNTE, CLISTN, COEFN, CO2TOP, RAD, SUNANG, XALT )
C:
