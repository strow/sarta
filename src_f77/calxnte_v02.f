C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore County [UMBC]
C
C    AIRS
C
C    CALNTE (for Non-local Thermodynamic Equilibrium)
C            extended solar angle range.
!F77====================================================================


!ROUTINE NAME:
C    CALNTE


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
C    Jun 2022 C Hepplewhite  Add extended range byond solzen>90 twilight.
!END====================================================================

C      =================================================================
       SUBROUTINE CALNTE ( INDCHN, TEMP, VSEC1,
     $    NCHNTE, CLISTN, COEFN, CO2TOP, RAD, SUNANG, XALT )
C$$$       SUBROUTINE CALNTE ( INDCHN, TEMP, SUNCOS, SCOS1, VSEC1,
C$$$         SUNCOS, SCOS1, VSEC1,
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
C      QIKEXP
C       REAL VACONV
       REAL SACONV


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      Input
       INTEGER INDCHN(MXCHAN)
       REAL   TEMP(MAXLAY)
       REAL SUNANG ! solar zenith angle from prof.solzen.
       REAL SZALAY ! solar zenith angle at layer1.
       REAL SUNCOS ! solar zenith angle cosine at surface
       REAL  SCOS1 ! solar zenith angle cosine at layer1
       REAL  VSEC1 ! satellite view zenith angle secant at layer1
       REAL  XALT
       INTEGER NCHNTE
       INTEGER CLISTN(MXCNTE)
       REAL  COEFN(NNCOEF,MXCNTE)
       REAL CO2TOP ! CO2 mixing ratio in top layers (ppmv)
C
C      Input/Output
       REAL    RAD(MXCHAN)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER      I
       INTEGER      J
       REAL   DRAD
       REAL  PRED1
       REAL  PRED2
       REAL  PRED3
       REAL  PRED4
       REAL  PRED5
       REAL  PRED6
       REAL  THIGH
       REAL XZALAY ! solar zenith angle at layer1.
       REAL XUNCOS ! solar zenith angle cosine at surface
       REAL  XCOS1 ! solar zenith angle cosine at layer1
       REAL XUNTNH ! solar zenith TANH
       REAL XTNH1  ! solz at lay 1 TANH
       REAL  CONV  ! degrees to radians
       CONV=1.7453292E-02
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

C      Recalculate SZALAY, SUNCOS, SCOS1 independent of sarta.f local
C      from SUNANG. Local vars prefix with X...
       XUNCOS = COS(CONV*SUNANG)
       XUNTNH = COS(CONV*SUNANG)
       XZALAY = SACONV(SUNANG,XALT)
       XCOS1  = COS(XZALAY)
       XTNH1  = TANH(XZALAY)
C       write(6,"('calxnte: SUNANG,SZALAY,XTNH1',F6.2,X,F11.3,X,F11.3 )") 
C     $  SUNANG,XZALAY,XTNH1
C      Calculate the channel independent non-LTE predictors
       THIGH = (TEMP(1) + TEMP(2) + TEMP(3) + TEMP(4) + TEMP(5))/5.0
       THIGH = THIGH/224.4                      ! rescaled from fitting
       PRED1 = 1.0
       PRED2 = XTNH1
       PRED3 = XTNH1*XTNH1
       PRED4 = XTNH1*VSEC1
       PRED5 = XTNH1*THIGH
       PRED6 = TANH(CONV*SUNANG)
C
CCC       PRED2 = SCOS1
CCC       PRED3 = SCOS1*SCOS1
CCC       PRED4 = SCOS1*VSEC1
CCC       PRED5 = SCOS1*THIGH
CCC       PRED6 = SUNCOS

C      ---------------------------
C      Loop on channel (frequency)
C      ---------------------------
       DO I=1,NCHNTE
C
C         Index for RAD
          J=INDCHN( CLISTN(I) )
C
          IF(SUNANG .LE. 90) THEN
            DRAD=( COEFN(1,I)*PRED1 ) +
     $           ( COEFN(2,I)*PRED2 ) +
     $           ( COEFN(3,I)*PRED3 ) +
     $           ( COEFN(4,I)*PRED4 ) +
     $           ( COEFN(5,I)*PRED5 ) +
     $           ( COEFN(6,I)*PRED6 )
          ELSEIF(SUNANG .GT. 90) THEN
            DRAD=( COEFN(8,I)*PRED1 ) +
     $           ( COEFN(9,I)*PRED2 ) +
     $           ( COEFN(10,I)*PRED3 ) +
     $           ( COEFN(11,I)*PRED4 ) +
     $           ( COEFN(12,I)*PRED5 ) +
     $           ( COEFN(13,I)*PRED6 )
          ENDIF
C
C         Adjust DRAD for CO2 mixing ratio
          DRAD=DRAD*(COEFN(7,I)*(CO2TOP - CO2NTE) + 1.0)
C
C         Adjust RAD for the non-LTE contribution
          RAD(J) = RAD(J) + DRAD/1000.0 ! convert DRAD to Watts
C
C       write(6,'(A,X,I3,X,E11.3)') 'calnte: I, XCO2 ',I,(COEFN(7,I)*(CO2TOP - CO2NTE) + 1.0)
C
       ENDDO
C      End loops on channel number (frequency)
C
       RETURN
       END
