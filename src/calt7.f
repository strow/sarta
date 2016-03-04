C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore County [UMBC]
C
C    AIRS
C
C    CALT7 (set7=FWO sun mfbw) version for trace gases (no SO2 or HNO3)
C
!F77====================================================================


!ROUTINE NAME:
C    CALT7


!ABSTRACT:
C    Calculate the transmittance for set7 using the predictors and the
C    fast transmittance coefficients.


!CALL PROTOCOL:
C    CALT7( LTAU, INDCHN, NLAY, BLMULT, NCHN7, CLIST7, COEF7,
C       FIXMUL, CONPD7, FPRED7, WPRED7, OPRED7, TRCPRD, INDCO2, COFCO2,
C       CO2MLT, INDN2O, COFN2O, N2OMLT, XZ, TAU, TAUZ )


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    LOGICAL   LTAU    calc all layer trans?       none
C    INT arr   INDCHN  channel indices             none
C    INTEGER   NLAY    number of layers to bottom  none
C    REAL      BLMULT  bottom layer opt depth mult none
C    INTEGER   NCHN7   set7 number of channels     none
C    INT arr   CLIST7  set7 channel list           none
C    REAL arr  COEF7   set7 fast trans coefs       various
C    REAL arr  FIXMUL  fixed amount mult (~1.0)    none
C    REAL arr  CONPD7  set7 H2O continuum preds    various
C    REAL arr  FPRED7  set7 fixed gases preds      various
C    REAL arr  WPRED7  set7 water predictors       various
C    REAL arr  OPRED7  set7 ozone predictors       various
C    REAL arr  TRCPRD  trace gas pert predictors   various
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
C    transmittances. Fixed, water, and ozone transmittances are each
C    checked individually to be sure they give 0 < trans < 1.
C
C    ===================================================================
C    Loops downward over all the layers for each of the NCHN7 channels
C    to compute the layer transmittances TAU.
C
C    The water continuum absorption coefficient is:
C       k_con = the sum i=1 to 5 of { COEF(i)*CONPRD(i) }
C
C    The layer effective fixed gas absorption coefficient is:
C       k_fixed = the sum i=1 to 8 of { COEF(5+i)*FPRED(i) }
C
C    The layer effective water lines absorption coefficient is:
C       k_water = the sum i=1 to 13 of { COEF(5+8+i)*WPRED(i) }
C
C    The layer effective ozone absorption coefficient is:
C       k_ozone = COEF(5+8+13+1)*OPRED(1)
C
C    where
C      "COEF" are the fast transmittance coefficients COEF4
C      "CONPRD" are the water continuum predictors CONPRD
C      "FPRED" are the fixed gases predictors FPRED
C      "WPRED" are the water lines predictors WPRED4
C      "OPRED" are the ozone predictors OPRED4
C
C    The total layer effective transmittance TAU is:
C       TAU = exp( -[ k_con + k_fixed + k_water + k_ozone ])
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
C  7 Jul 1997 Scott Hannon   Created for set7
C  3 Sep 1997 Scott Hannon   Added TAUZ and BLMULT
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
       SUBROUTINE CALT7 ( LTAU, INDCHN, NLAY, BLMULT, NCHN7, CLIST7,
     $    COEF7, FIXMUL, CONPD7, FPRED7, WPRED7, OPRED7, TRCPRD, INDCO2,
     $    COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT, XZ, TAU, TAUZ )
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
       INTEGER  NCHN7
       INTEGER CLIST7(MXCHN7)
       REAL  COEF7(N7COEF,MAXLAY,MXCHN7)
       REAL FIXMUL(MAXLAY)
       REAL CONPD7( N7CON,MAXLAY)
       REAL FPRED7( N7FIX,MAXLAY)
       REAL WPRED7( N7H2O,MAXLAY)
       REAL OPRED7(  N7O3,MAXLAY)
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
       REAL   KCON
       REAL   KFIX
       REAL   KOZO
       REAL KLAYER
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
       DO I=1,NCHN7
C
C         Index for TAU
          J=INDCHN( CLIST7(I) )
C
C         Determine whether or not to do variable CO2
          ICO2=INDCO2( CLIST7(I) )
          IF (ICO2 .GT. 0) THEN
             LCO2=.TRUE.
          ELSE
             LCO2=.FALSE.
          ENDIF
C
C         Determine whether or not to do variable CO2
          IN2O=INDN2O( CLIST7(I) )
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
             KCON=( COEF7(1,ILAY,I)*CONPD7(1,ILAY) ) +
     $            ( COEF7(2,ILAY,I)*CONPD7(2,ILAY) ) +
     $            ( COEF7(3,ILAY,I)*CONPD7(3,ILAY) ) +
     $            ( COEF7(4,ILAY,I)*CONPD7(4,ILAY) ) +
     $            ( COEF7(5,ILAY,I)*CONPD7(5,ILAY) ) +
     $            ( COEF7(6,ILAY,I)*CONPD7(6,ILAY) ) +
     $            ( COEF7(7,ILAY,I)*CONPD7(7,ILAY) )
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
             KFIX=( COEF7( 8,ILAY,I)*FPRED7( 1,ILAY) ) +
     $            ( COEF7( 9,ILAY,I)*FPRED7( 2,ILAY) ) +
     $            ( COEF7(10,ILAY,I)*FPRED7( 3,ILAY) ) +
     $            ( COEF7(11,ILAY,I)*FPRED7( 4,ILAY) ) +
     $            ( COEF7(12,ILAY,I)*FPRED7( 5,ILAY) ) +
     $            ( COEF7(13,ILAY,I)*FPRED7( 6,ILAY) ) +
     $            ( COEF7(14,ILAY,I)*FPRED7( 7,ILAY) ) +
     $            ( COEF7(15,ILAY,I)*FPRED7( 8,ILAY) )
C
             KFIX=KFIX*FIXMUL(ILAY)
C
             IF (KFIX .LT. 0.0E+0) THEN
                KFIX=0.0E+0
             ELSEIF (KFIX .GT. 1.0E+1) THEN
                KFIX=1.0E+1
             ENDIF
C
C
C            --------------------------
C            Compute the water abs coef
C            --------------------------
             KWAT=( COEF7(16,ILAY,I)*WPRED7( 1,ILAY) ) +
     $            ( COEF7(17,ILAY,I)*WPRED7( 2,ILAY) ) +
     $            ( COEF7(18,ILAY,I)*WPRED7( 3,ILAY) ) +
     $            ( COEF7(19,ILAY,I)*WPRED7( 4,ILAY) ) +
     $            ( COEF7(20,ILAY,I)*WPRED7( 5,ILAY) ) +
     $            ( COEF7(21,ILAY,I)*WPRED7( 6,ILAY) ) +
     $            ( COEF7(22,ILAY,I)*WPRED7( 7,ILAY) ) +
     $            ( COEF7(23,ILAY,I)*WPRED7( 8,ILAY) ) +
     $            ( COEF7(24,ILAY,I)*WPRED7( 9,ILAY) ) +
     $            ( COEF7(25,ILAY,I)*WPRED7(10,ILAY) ) +
     $            ( COEF7(26,ILAY,I)*WPRED7(11,ILAY) ) +
     $            ( COEF7(27,ILAY,I)*WPRED7(12,ILAY) ) +
     $            ( COEF7(28,ILAY,I)*WPRED7(13,ILAY) )
C
             IF (KWAT .LT. 0.0E+0) THEN
                KWAT=0.0E+0
             ELSEIF( KWAT .GT. 1.0E+1) THEN
                KWAT=1.0E+1
             ENDIF
C
C
C            --------------------------
C            Compute the ozone abs coef
C            --------------------------
             KOZO=( COEF7(29,ILAY,I)*OPRED7(1,ILAY) )
C
             IF (KOZO .LT. 0.0E+0) THEN
                KOZO=0.0E+0
             ELSEIF (KOZO .GT. 1.0E+1) THEN
                KOZO=1.0E+1
             ENDIF
C
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
c           kwat=0.0E+0
c           kozo=0.0E+0
ccccc
C
C            ----------------------------
C            Calc change in total optical
C            depth due to variable CO2
C            ----------------------------
             IF (LCO2 .AND. CO2MLT(ILAY) .NE. 0.0) THEN
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
             IF (LN2O .AND. N2OMLT(ILAY) .NE. 0.0) THEN
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
             KLAYER=KCON + KFIX + KWAT + KOZO + DK
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
