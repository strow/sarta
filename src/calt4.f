C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore County [UMBC]
C
C    AIRS
C
C    CALT4 (for set4 = FCOW) version with trace gases (no SO2 or HNO3)
C
!F77====================================================================


!ROUTINE NAME:
C    CALT4


!ABSTRACT:
C    Calculate the transmittance for set4 using the predictors and the
C    fast transmittance coefficients.


!CALL PROTOCOL:
C    CALT4( LTAU, INDCHN, NLAY, BLMULT, NCHN4, CLIST4, COEF4,
C       FIXMUL, CONPD4, FPRED4, CPRED4, OPRED4, WPRED4, TRCPRD, INDCO2,
C       COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT, XZ, TAU, TAUZ )


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    LOGICAL   LTAU    Calc all layer trans?       none
C    INT arr   INDCHN  channel indices             none
C    INTEGER   NLAY    number of layers to bottom  none
C    REAL      BLMULT  bottom layer opt depth mult none
C    INTEGER   NCHN4   set4 number of channels     none
C    INT arr   CLIST4  set4 channel list           none
C    REAL arr  COEF4   set4 fast trans coefs       various
C    REAL arr  FIXMUL  fixed amount mult (~1.0)    none
C    REAL arr  CONPD4  set4 H2O continuum preds    various
C    REAL arr  FPRED4  set4 fixed gases preds      various
C    REAL arr  CPRED4  set4 carbon monoxide preds  various
C    REAL arr  OPRED4  set4 ozone predictors       various
C    REAL arr  WPRED4  set4 water predictors       various
C    REAL arr  TRCPRD  trace gases pert predictors various
C    INT arr   INDCO2  CO2 pert chan indices       none
C    REAL arr  COFCO2  CO2 pert coefs              various
C    REAL arr  CO2MLT  CO2 pert multiplier         none
C    INT arr   INDN2O  N2O pert chan indices       none
C    REAL arr  COFN2O  N2O pert coefs              various
C    REAL arr  N2OMLT  N2O pert multiplier         none
C    REAL      XZ      optical depth mult for TAUZ none


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    REAL arr  TAU     effective layer trans       none
C    REAL arr  TAUZ    layer-to-space trans        none


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
C    August 2000 version of the 100 layer AIRS Fast Transmittance
C    Code by L.L.Strow/S.Hannon.
C
C    The fast trans coefficents and predictors are multiplied
C    together and summed to calculate the effective layer
C    transmittances. Fixed, CO, ozone, and water transmittances are
C    each checked individually to be sure they give 0 < trans < 1.
C
C    ===================================================================
C    Loops downward over all the layers for each of the NCHN4 channels
C    to compute the layer transmittances TAU.
C
C    The water continuum absorption coefficient is:
C       k_con = the sum i=1 to 5 of { COEF(i)*CONPRD(i) }
C
C    The layer effective fixed gas absorption coefficient is:
C       k_fixed = the sum i=1 to 8 of { COEF(5+i)*FPRED(i) }
C
C    The layer effective CO absorption coefficient is:
C       k_co = the sum i=1 to 9 of { COEF(5+8+i)*OPRED(i) }
C
C    The layer effective ozone absorption coefficient is:
C       k_ozone = the sum i=1 to 3 of { COEF(5+8+9+i)*OPRED(i) }
C
C    The layer effective water lines absorption coefficient is:
C       k_water = the sum i=1 to 11 of { COEF(5+8+9+3+i)*WPRED(i) }
C
C    where
C      "COEF" are the fast transmittance coefficients COEF4
C      "CONPRD" are the water continuum predictors CONPRD
C      "FPRED" are the fixed gases predictors FPRED4
C      "CPRED" are the carbon monoxide predictors CPRED4
C      "OPRED" are the ozone predictors OPRED4
C      "WPRED" are the water lines predictors WPRED4
C
C    The total layer effective transmittance is:
C       TAU = exp( -[ k_con + k_fixed + k_co + k_ozone + k_water])
C    TAU is only calc'ed if LTAU is TRUE.
C
C    To help speed up the exponential calculations, we use our own
C    "EXP" replacement function called QIKEXP which uses just the
C    first few series expansion terms for exp(x) if x is suitably small.
C    ===================================================================


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C Date        Programmer     Comments
C ----------- -------------- ----------------------------------------
C  1 Dec 1994 Scott Hannon   Created
C  3 Feb 1997 Scott Hannon   Re-wrote (from CALTAU) for FCOW
C  3 Sep 1997 Scott Hannon   Re-wrote for sun and added TAUZ & BLMULT
C 30 Sep 1997 Scott Hannon   Added variable CO2
C 27 Feb 1998 Scott Hannon   Added LTAU
C 11 Aug 2000 Scott Hannon   Change from 4 to 5 term H2O continuum
C 12 Sep 2002 Scott Hannon   Add predictors 6 & 7 to H2O con
C  3 Jan 2003 Scott Hannon   Add XZ
C 12 Oct 2004 Scott Hannon   Change CO2MLT from scaler to vector
C 28 Jun 2005 Scott Hannon   "trace" version for CO2,N2O
C 30 Apr 2008 Scott Hannon   Change CO2 from 4 to 5 predictors

!END====================================================================

C      =================================================================
       SUBROUTINE CALT4 ( LTAU, INDCHN, NLAY, BLMULT, NCHN4, CLIST4,
     $    COEF4, FIXMUL, CONPD4, FPRED4, CPRED4, OPRED4, WPRED4, TRCPRD,
     $    INDCO2, COFCO2, CO2MLT,INDN2O, COFN2O, N2OMLT, XZ, TAU, TAUZ )
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


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      Input
       LOGICAL   LTAU
       INTEGER INDCHN(MXCHAN)
       INTEGER   NLAY
       REAL BLMULT
       INTEGER  NCHN4
       INTEGER CLIST4(MXCHN4)
       REAL  COEF4(N4COEF,MAXLAY,MXCHN4)
       REAL FIXMUL(MAXLAY)
       REAL CONPD4( N4CON,MAXLAY)
       REAL FPRED4( N4FIX,MAXLAY)
       REAL CPRED4(  N4CO,MAXLAY)
       REAL OPRED4(  N4O3,MAXLAY)
       REAL WPRED4( N4H2O,MAXLAY)
       REAL TRCPRD(NTRACE,MAXLAY)
       INTEGER INDCO2(MXCHAN)
       REAL COFCO2(  NCO2,MAXLAY,MXCHNC)
       REAL CO2MLT(MAXLAY)
       INTEGER INDN2O(MXCHAN)
       REAL COFN2O(  NN2O,MAXLAY,MXCHNN)
       REAL N2OMLT(MAXLAY)
       REAL     XZ
C
C      Output
       REAL    TAU(MAXLAY,MXCHAN)
       REAL   TAUZ(MXCHAN)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER      I
       INTEGER   ICO2
       INTEGER   ILAY
       INTEGER   IN2O
       INTEGER      J
       REAL     DK
       REAL  DKCO2
       REAL  DKN2O
       REAL    KCO
       REAL   KCON
       REAL   KFIX
       REAL KLAYER
       REAL   KOZO
       REAL   KWAT
       REAL     KZ
       LOGICAL   LCO2
       LOGICAL   LN2O
C
C      for function QIKEXP
       REAL QIKEXP


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
C      ---------------------------
C      Loop on channel (frequency)
C      ---------------------------
       DO I=1,NCHN4
C
C         Index for TAU
          J=INDCHN( CLIST4(I) )
C
C         Determine whether or not to do variable CO2
          ICO2=INDCO2( CLIST4(I) )
          IF (ICO2 .GT. 0) THEN
             LCO2=.TRUE.
          ELSE
             LCO2=.FALSE.
          ENDIF
C
C         Determine whether or not to do variable N2O
          IN2O=INDN2O( CLIST4(I) )
          IF (IN2O .GT. 0) THEN
             LN2O=.TRUE.
          ELSE
             LN2O=.FALSE.
          ENDIF
C
C         Initialize the layer-to-space optical depth
          KZ=0.0E+0
C 
C         ------------------------------
C         Loop on layers (top to ground)
C         ------------------------------
          DO ILAY=1,NLAY
C
C            ---------------------------
C            Compute the water continuum
C            ---------------------------
             KCON=( COEF4(1,ILAY,I)*CONPD4(1,ILAY) ) +
     $            ( COEF4(2,ILAY,I)*CONPD4(2,ILAY) ) +
     $            ( COEF4(3,ILAY,I)*CONPD4(3,ILAY) ) +
     $            ( COEF4(4,ILAY,I)*CONPD4(4,ILAY) ) +
     $            ( COEF4(5,ILAY,I)*CONPD4(5,ILAY) ) +
     $            ( COEF4(6,ILAY,I)*CONPD4(6,ILAY) ) +
     $            ( COEF4(7,ILAY,I)*CONPD4(7,ILAY) )
C
             IF (KCON .LT. 0.0E+0) THEN
                KCON=0.0E+0
             ELSEIF (KCON .GT. 1.0E+1) THEN
                KCON=1.0E+1
             ENDIF
C
C            -----------------------------
C            Calc the fixed gases abs coef
C            -----------------------------
             KFIX=( COEF4( 8,ILAY,I)*FPRED4( 1,ILAY) ) +
     $            ( COEF4( 9,ILAY,I)*FPRED4( 2,ILAY) ) +
     $            ( COEF4(10,ILAY,I)*FPRED4( 3,ILAY) ) +
     $            ( COEF4(11,ILAY,I)*FPRED4( 4,ILAY) ) +
     $            ( COEF4(12,ILAY,I)*FPRED4( 5,ILAY) ) +
     $            ( COEF4(13,ILAY,I)*FPRED4( 6,ILAY) ) +
     $            ( COEF4(14,ILAY,I)*FPRED4( 7,ILAY) ) +
     $            ( COEF4(15,ILAY,I)*FPRED4( 8,ILAY) ) +
     $            ( COEF4(16,ILAY,I)*FPRED4( 9,ILAY) ) +
     $            ( COEF4(17,ILAY,I)*FPRED4(10,ILAY) ) +
     $            ( COEF4(18,ILAY,I)*FPRED4(11,ILAY) )
C
             KFIX=KFIX*FIXMUL(ILAY)
C
             IF (KFIX .LT. 0.0E+0) THEN
                KFIX=0.0E+0
             ELSEIF (KFIX .GT. 1.0E+1) THEN
                KFIX=1.0E+1
             ENDIF
C
C            -----------------------
C            Compute the CO abs coef
C            -----------------------
             KCO=( COEF4(19,ILAY,I)*CPRED4( 1,ILAY) ) +
     $           ( COEF4(20,ILAY,I)*CPRED4( 2,ILAY) ) +
     $           ( COEF4(21,ILAY,I)*CPRED4( 3,ILAY) ) +
     $           ( COEF4(22,ILAY,I)*CPRED4( 4,ILAY) ) +
     $           ( COEF4(23,ILAY,I)*CPRED4( 5,ILAY) ) +
     $           ( COEF4(24,ILAY,I)*CPRED4( 6,ILAY) ) +
     $           ( COEF4(25,ILAY,I)*CPRED4( 7,ILAY) ) +
     $           ( COEF4(26,ILAY,I)*CPRED4( 8,ILAY) ) +
     $           ( COEF4(27,ILAY,I)*CPRED4( 9,ILAY) ) +
     $           ( COEF4(28,ILAY,I)*CPRED4(10,ILAY) ) +
     $           ( COEF4(29,ILAY,I)*CPRED4(11,ILAY) )
C
             IF (KCO .LT. 0.0E+0) THEN
                KCO=0.0E+0
             ELSEIF (KCO .GT. 1.0E+1) THEN
                KCO=1.0E+1
             ENDIF
C
C            --------------------------
C            Compute the ozone abs coef
C            --------------------------
             KOZO=( COEF4(30,ILAY,I)*OPRED4(1,ILAY) ) +
     $            ( COEF4(31,ILAY,I)*OPRED4(2,ILAY) ) +
     $            ( COEF4(32,ILAY,I)*OPRED4(3,ILAY) )
C
             IF (KOZO .LT. 0.0E+0) THEN
                KOZO=0.0E+0
             ELSEIF (KOZO .GT. 1.0E+1) THEN
                KOZO=1.0E+1
             ENDIF
C
C            --------------------------
C            Compute the water abs coef
C            --------------------------
             KWAT=( COEF4(33,ILAY,I)*WPRED4( 1,ILAY) ) +
     $            ( COEF4(34,ILAY,I)*WPRED4( 2,ILAY) ) +
     $            ( COEF4(35,ILAY,I)*WPRED4( 3,ILAY) ) +
     $            ( COEF4(36,ILAY,I)*WPRED4( 4,ILAY) ) +
     $            ( COEF4(37,ILAY,I)*WPRED4( 5,ILAY) ) +
     $            ( COEF4(38,ILAY,I)*WPRED4( 6,ILAY) ) +
     $            ( COEF4(39,ILAY,I)*WPRED4( 7,ILAY) ) +
     $            ( COEF4(40,ILAY,I)*WPRED4( 8,ILAY) ) +
     $            ( COEF4(41,ILAY,I)*WPRED4( 9,ILAY) ) +
     $            ( COEF4(42,ILAY,I)*WPRED4(10,ILAY) ) +
     $            ( COEF4(43,ILAY,I)*WPRED4(11,ILAY) ) +
     $            ( COEF4(44,ILAY,I)*WPRED4(12,ILAY) ) +
     $            ( COEF4(45,ILAY,I)*WPRED4(13,ILAY) )
C
             IF (KWAT .LT. 0.0E+0) THEN
                KWAT=0.0E+0
             ELSEIF( KWAT .GT. 1.0E+1) THEN
                KWAT=1.0E+1
             ENDIF
C
C            ----------------------------------
C            Calc the total layer transmittance
C            ----------------------------------
c
ccccc
c This block is usually commented out and is only uncommented for
c testing purposes.
c
c           kcon=0.0E+0
c           kfix=0.0E+0
c           kco =0.0E+0
c           kozo=0.0E+0
c           kwat=0.0E+0
ccccc
C
C            ----------------------------
C            Calc change in total optical
C            depth due to variable CO2
C            ----------------------------
             IF (LCO2 .AND. CO2MLT(ILAY) .NE. 0) THEN
                DKCO2=( COFCO2(1,ILAY,ICO2)*TRCPRD(1,ILAY) ) +
     $                ( COFCO2(2,ILAY,ICO2)*TRCPRD(2,ILAY) ) +
     $                ( COFCO2(3,ILAY,ICO2)*TRCPRD(3,ILAY) ) +
     $                ( COFCO2(4,ILAY,ICO2)*TRCPRD(4,ILAY) ) +
     $                ( COFCO2(5,ILAY,ICO2)*TRCPRD(5,ILAY) )
                DKCO2=DKCO2*CO2MLT(ILAY)
             ELSE
                DKCO2=0.0
             ENDIF
C
C            ----------------------------
C            Calc change in total optical
C            depth due to variable N2O
C            ----------------------------
             IF (LN2O .AND. N2OMLT(ILAY) .NE. 0) THEN
                DKN2O=( COFN2O(1,ILAY,IN2O)*TRCPRD(1,ILAY) ) +
     $                ( COFN2O(2,ILAY,IN2O)*TRCPRD(2,ILAY) ) +
     $                ( COFN2O(3,ILAY,IN2O)*TRCPRD(3,ILAY) ) +
     $                ( COFN2O(4,ILAY,IN2O)*TRCPRD(4,ILAY) ) +
     $                ( COFN2O(5,ILAY,IN2O)*TRCPRD(5,ILAY) ) +
     $                ( COFN2O(6,ILAY,IN2O)*TRCPRD(6,ILAY) ) +
     $                ( COFN2O(7,ILAY,IN2O)*TRCPRD(7,ILAY) )
                DKN2O=DKN2O*N2OMLT(ILAY)
             ELSE
                DKN2O=0.0
             ENDIF
C
ccc
c this block for testing
c      DKCO2=0.0
c      DKN2O=0.0
ccc
C            Limit -DK so it can never totally totally cancel KFIX
             DK = DKCO2 + DKN2O
             IF (-DK .GE. KFIX) THEN
                DK = -0.999*KFIX
             ENDIF

C            Calc total layer optical depth
             KLAYER = KCON + KFIX + KCO + KOZO + KWAT + DK
C
C            Adjust the optical depth of the bottom layer
             IF (ILAY .EQ. NLAY) KLAYER=BLMULT*KLAYER
C
C            Calc layer-to-space optical depth
             KZ=KZ + KLAYER
C
C            Calc effective layer transmittance
             IF (LTAU) TAU(ILAY,J)=QIKEXP(-KLAYER)
C
          ENDDO
C         End loop on levels
C
C         Convert KZ to TAUZ
          TAUZ(J)=QIKEXP(-KZ*XZ)
C
       ENDDO
C      End loops on channel number (frequency)
C
       RETURN
       END
