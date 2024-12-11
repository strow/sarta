!=======================================================================
!
!    University of Maryland Baltimore County [UMBC]
!
!    AIRS
!
!    CALT5 (set5=FWO sun bfsw) version with trace gases (no SO2 or HNO3)
!
!F90====================================================================


!ROUTINE NAME:
!    CALT5


!ABSTRACT:
!    Calculate the transmittance for set5 using the predictors and the
!    fast transmittance coefficients.


!CALL PROTOCOL:
!    CALT5( LTAU, INDCHN, NLAY, BLMULT, NCHN5, CLIST5, COEF5,
!       FIXMUL, CONPD5, FPRED5, WPRED5, OPRED5, TRCPRD, INDCO2, COFCO2,
!       CO2MLT, INDN2O, COFN2O, N2OMLT, XZ, TAU, TAUZ )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    LOGICAL   LTAU    Calc all layer trans?       none
!    INT arr   INDCHN  channel indices             none
!    INTEGER   NLAY    number of layers to bottom  none
!    REAL      BLMULT  bottom layer opt depth mult none
!    INTEGER   NCHN5   set5 number of channels     none
!    INT arr   CLIST5  set5 channel list           none
!    REAL arr  COEF5   set5 fast trans coefs       various
!    REAL arr  FIXMUL  fixed amount mult (~1.0)    none
!    REAL arr  CONPD5  set5 H2O continuum preds    various
!    REAL arr  FPRED5  set5 fixed gases preds      various
!    REAL arr  WPRED5  set5 water predictors       various
!    REAL arr  OPRED5  set5 ozone predictors       various
!    REAL arr  TRCPRD  trace gas pert predictors   various
!    INT arr   INDCO2  CO2 pert chan indices       none
!    REAL arr  COFCO2  CO2 pert coefs              various
!    REAL arr  CO2MLT  CO2 pert multiplier         none
!    INT arr   INDN2O  N2O pert chan indices       none
!    REAL arr  COFN2O  N2O pert coefs              various
!    REAL arr  N2OMLT  N2O pert multiplier         none
!    REAL      XZ      optical depth mult for TAUZ none


!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL arr  TAU     effective layer trans       none
!    REAL arr  TAUZ    layer-to-space trans        none


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
!    August 2000 version of the 100 layer AIRS Fast Transmittance
!    Code by L.L.Strow/S.Hannon.
!
!    The fast trans coefficents and predictors are multiplied
!    together and summed to calculate the effective layer
!    transmittances. Fixed, water, and ozone transmittances are each
!    checked individually to be sure they give 0 < trans < 1.
!
!    ===================================================================
!    Loops downward over all the layers for each of the NCHN5 channels
!    to compute the layer transmittances TAU.
!
!    The water continuum absorption coefficient is:
!       k_con = the sum i=1 to 5 of { COEF(i)*CONPRD(i) }
!
!    The layer effective fixed gas absorption coefficient is:
!       k_fixed = the sum i=1 to 11 of { COEF(5+i)*FPRED(i) }
!
!    The layer effective water lines absorption coefficient is:
!       k_water = the sum i=1 to 3 of { COEF(5+11+i)*WPRED(i) }
!
!    The layer effective ozone absorption coefficient is:
!       k_ozone = COEF(5+11+3+1)*OPRED(1)
!
!    where
!      "COEF" are the fast transmittance coefficients COEF5
!      "CONPRD" are the water continuum predictors CONPRD
!      "FPRED" are the fixed gases predictors FPRED5
!      "WPRED" are the water lines predictors WPRED5
!      "OPRED" are the ozone predictors OPRED5
!
!    The total layer effective transmittance TAU is:
!       TAU = exp( -[ k_con + k_fixed + k_water + k_ozone ])
!    TAU is only calc'ed if LTAU is TRUE.
!
!    To help speed up the exponential calculations, we use our own
!    "EXP" replacement function called QIKEXP which uses just the
!    first few series expansion terms for exp(x) if x is suitably small.
!    ===================================================================


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    none


!ROUTINE HISTORY:
! Date        Programmer     Comments
! ----------- -------------- ----------------------------------------
! 26 Jun 1997 Scott Hannon   Created for set5
!  3 Sep 1997 Scott Hannon   Added TAUZ and BLMULT
! 30 Sep 1997 Scott Hannon   Added variable CO2
! 27 Feb 1998 Scott Hannon   Added LTAU
! 11 Aug 2000 Scott Hannon   Change from 4 to 5 term H2O continuum
! 12 Sep 2002 Scott Hannon   Add predictors 6 & 7 to H2O con
!  3 Jan 2003 Scott Hannon   Add XZ
! 12 Oct 2004 Scott Hannon   Change CO2MLT from scaler to vector
! 28 Jun 2005 Scott Hannon   "trace" version for CO2,N2O
! 30 Apr 2008 Scott Hannon   Change CO2 from 4 to 5 predictors

!END====================================================================

!      =================================================================
       SUBROUTINE CALT5 ( LTAU, INDCHN, NLAY, BLMULT, NCHN5, CLIST5, &
        COEF5, FIXMUL, CONPD5, FPRED5, WPRED5, OPRED5, TRCPRD, INDCO2, &
        COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT, XZ, TAU, TAUZ )
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
!      QIKEXP

!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
!      Input
       LOGICAL   LTAU
       INTEGER INDCHN(MXCHAN)
       INTEGER   NLAY
       REAL BLMULT
       INTEGER  NCHN5
       INTEGER CLIST5(MXCHN5)
       REAL  COEF5(N5COEF,MAXLAY,MXCHN5)
       REAL FIXMUL(MAXLAY)
       REAL CONPD5( N5CON,MAXLAY)
       REAL FPRED5( N5FIX,MAXLAY)
       REAL WPRED5( N5H2O,MAXLAY)
       REAL OPRED5(  N5O3,MAXLAY)
       REAL TRCPRD(NTRACE,MAXLAY)
       INTEGER INDCO2(MXCHAN)
       REAL COFCO2(  NCO2,MAXLAY,MXCHNC)
       REAL CO2MLT(MAXLAY)
       INTEGER INDN2O(MXCHAN)
       REAL COFN2O(  NN2O,MAXLAY,MXCHNN)
       REAL N2OMLT(MAXLAY)
       REAL     XZ
!
!      Output
       REAL    TAU(MAXLAY,MXCHAN)
       REAL   TAUZ(MXCHAN)


!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
integer ::  I, ICO2, ILAY, IN2O, J
real(4) ::  DK, DKCO2, DKN2O, KCON, KFIX, KLAYER, KOZO, KWAT, KZ
logical ::  LCO2, LN2O
!
!      for function QIKEXP
       REAL QIKEXP

!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none


!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!
!      ---------------------------
!      Loop on channel (frequency)
!      ---------------------------
       DO I=1,NCHN5
!
!         Index for TAU
          J=INDCHN( CLIST5(I) )
!
!         Determine whether or not to do variable CO2
          ICO2=INDCO2( CLIST5(I) )
          IF (ICO2 .GT. 0) THEN
             LCO2=.TRUE.
          ELSE
             LCO2=.FALSE.
          ENDIF
!
!         Determine whether or not to do variable CO2
          IN2O=INDN2O( CLIST5(I) )
          IF (IN2O .GT. 0) THEN
             LN2O=.TRUE.
          ELSE
             LN2O=.FALSE.
          ENDIF
!
!         Initialize the layer-to-space optical depth
          KZ=0.0E+0
!
!         ------------------------------
!         Loop on layers (top to ground)
!         ------------------------------
          DO ILAY=1,NLAY
!
!            ---------------------------
!            Compute the water continuum
!            ---------------------------
             KCON=( COEF5(1,ILAY,I)*CONPD5(1,ILAY) ) + &
                  ( COEF5(2,ILAY,I)*CONPD5(2,ILAY) ) + &
                  ( COEF5(3,ILAY,I)*CONPD5(3,ILAY) ) + &
                  ( COEF5(4,ILAY,I)*CONPD5(4,ILAY) ) + &
                  ( COEF5(5,ILAY,I)*CONPD5(5,ILAY) ) + &
                  ( COEF5(6,ILAY,I)*CONPD5(6,ILAY) ) + &
                  ( COEF5(7,ILAY,I)*CONPD5(7,ILAY) )
!
             IF (KCON .LT. 0.0E+0) THEN
                KCON=0.0E+0
             ELSEIF (KCON .GT. 1.0E+1) THEN
                KCON=1.0E+1
             ENDIF
!
!            -----------------------------
!            Calc the fixed gases abs coef
!            -----------------------------
             KFIX=( COEF5( 8,ILAY,I)*FPRED5( 1,ILAY) ) + &
                  ( COEF5( 9,ILAY,I)*FPRED5( 2,ILAY) ) + &
                  ( COEF5(10,ILAY,I)*FPRED5( 3,ILAY) ) + &
                  ( COEF5(11,ILAY,I)*FPRED5( 4,ILAY) ) + &
                  ( COEF5(12,ILAY,I)*FPRED5( 5,ILAY) ) + &
                  ( COEF5(13,ILAY,I)*FPRED5( 6,ILAY) ) + &
                  ( COEF5(14,ILAY,I)*FPRED5( 7,ILAY) ) + &
                  ( COEF5(15,ILAY,I)*FPRED5( 8,ILAY) ) + &
                  ( COEF5(16,ILAY,I)*FPRED5( 9,ILAY) ) + &
                  ( COEF5(17,ILAY,I)*FPRED5(10,ILAY) ) + &
                  ( COEF5(18,ILAY,I)*FPRED5(11,ILAY) )
!
             KFIX=KFIX*FIXMUL(ILAY)
!
             IF (KFIX .LT. 0.0E+0) THEN
                KFIX=0.0E+0
             ELSEIF (KFIX .GT. 1.0E+1) THEN
                KFIX=1.0E+1
             ENDIF
!
!
!            --------------------------
!            Compute the water abs coef
!            --------------------------
             KWAT=( COEF5(19,ILAY,I)*WPRED5( 1,ILAY) ) + &
                  ( COEF5(20,ILAY,I)*WPRED5( 2,ILAY) ) + &
                  ( COEF5(21,ILAY,I)*WPRED5( 3,ILAY) )
!
             IF (KWAT .LT. 0.0E+0) THEN
                KWAT=0.0E+0
             ELSEIF( KWAT .GT. 1.0E+1) THEN
                KWAT=1.0E+1
             ENDIF
!
!
!            --------------------------
!            Compute the ozone abs coef
!            --------------------------
             KOZO=( COEF5(22,ILAY,I)*OPRED5(1,ILAY) )
!
             IF (KOZO .LT. 0.0E+0) THEN
                KOZO=0.0E+0
             ELSEIF (KOZO .GT. 1.0E+1) THEN
                KOZO=1.0E+1
             ENDIF
!
!
!            ----------------------------------
!            Calc the total layer transmittance
!            ----------------------------------
!
!cccc
! This block is usually commented out and is only uncommented for
! testing purposes.
!
!           kcon=0.0E+0
!           kfix=0.0E+0
!           kwat=0.0E+0
!           kozo=0.0E+0
!cccc
!
!            ----------------------------
!            Calc change in total optical
!            depth due to variable CO2
!            ----------------------------
             IF (LCO2 .AND. CO2MLT(ILAY) .NE. 0.0) THEN
                DKCO2=( COFCO2(1,ILAY,ICO2)*TRCPRD(1,ILAY) ) + &
                      ( COFCO2(2,ILAY,ICO2)*TRCPRD(2,ILAY) ) + &
                      ( COFCO2(3,ILAY,ICO2)*TRCPRD(3,ILAY) ) + &
                      ( COFCO2(4,ILAY,ICO2)*TRCPRD(4,ILAY) ) + &
                      ( COFCO2(5,ILAY,ICO2)*TRCPRD(5,ILAY) )
                DKCO2=DKCO2*CO2MLT(ILAY)
             ELSE
                DKCO2=0.0
             ENDIF
!
!            ----------------------------
!            Calc change in total optical
!            depth due to variable N2O
!            ----------------------------
             IF (LN2O .AND. N2OMLT(ILAY) .NE. 0.0) THEN
                DKN2O=( COFN2O(1,ILAY,IN2O)*TRCPRD(1,ILAY) ) + &
                      ( COFN2O(2,ILAY,IN2O)*TRCPRD(2,ILAY) ) + &
                      ( COFN2O(3,ILAY,IN2O)*TRCPRD(3,ILAY) ) + &
                      ( COFN2O(4,ILAY,IN2O)*TRCPRD(4,ILAY) ) + &
                      ( COFN2O(5,ILAY,IN2O)*TRCPRD(5,ILAY) ) + &
                      ( COFN2O(6,ILAY,IN2O)*TRCPRD(6,ILAY) ) + &
                      ( COFN2O(7,ILAY,IN2O)*TRCPRD(7,ILAY) )
                DKN2O=DKN2O*N2OMLT(ILAY)
             ELSE
                DKN2O=0.0
             ENDIF
!
!cc
! this block for testing
!      DKCO2=0.0
!      DKN2O=0.0
!cc
!            Limit -DK so it can never totally totally cancel KFIX
             DK = DKCO2 + DKN2O
             IF (-DK .GE. KFIX) THEN
                DK = -0.999*KFIX
             ENDIF

!            Calc total layer optical depth
             KLAYER = KCON + KFIX + KWAT + KOZO + DK
!
!            Adjust the optical depth of the bottom layer
             IF (ILAY .EQ. NLAY) KLAYER=BLMULT*KLAYER
!
!            Calc layer-to-space optical depth
             KZ=KZ + KLAYER
!
!            Calc effective layer transmittance
             IF (LTAU) TAU(ILAY,J)=QIKEXP(-KLAYER)
!
          ENDDO
!         End loop on levels
!
!         Convert KZ to TAUZ
          TAUZ(J)=QIKEXP(-KZ*XZ)
!
       ENDDO
!      End loops on channel number (frequency)
!
       RETURN
       END
