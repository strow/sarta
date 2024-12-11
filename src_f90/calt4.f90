!=======================================================================
!
!    University of Maryland Baltimore County [UMBC]
!
!    AIRS
!
!    CALT4 (for set4 = FCOW) version with trace gases 
!          (no SO2 or HNO3 or NH3 or HDO)
!
!F90====================================================================


!ROUTINE NAME:
!    CALT4


!ABSTRACT:
!    Calculate the transmittance for set4 using the predictors and the
!    fast transmittance coefficients.


!CALL PROTOCOL:
!    CALT4( LTAU, INDCHN, NLAY, BLMULT, NCHN4, CLIST4, COEF4,
!       FIXMUL, CONPD4, FPRED4, CPRED4, OPRED4, WPRED4, TRCPRD, 
!       INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT, XZ, 
!       TAU, TAUZ )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    LOGICAL   LTAU    Calc all layer trans?       none
!    INT arr   INDCHN  channel indices             none
!    INTEGER   NLAY    number of layers to bottom  none
!    REAL      BLMULT  bottom layer opt depth mult none
!    INTEGER   NCHN4   set4 number of channels     none
!    INT arr   CLIST4  set4 channel list           none
!    REAL arr  COEF4   set4 fast trans coefs       various
!    REAL arr  FIXMUL  fixed amount mult (~1.0)    none
!    REAL arr  CONPD4  set4 H2O continuum preds    various
!    REAL arr  FPRED4  set4 fixed gases preds      various
!    REAL arr  CPRED4  set4 carbon monoxide preds  various
!    REAL arr  OPRED4  set4 ozone predictors       various
!    REAL arr  WPRED4  set4 water predictors       various
!    REAL arr  TRCPRD  trace gases pert predictors various
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
!    transmittances. Fixed, CO, ozone, and water transmittances are
!    each checked individually to be sure they give 0 < trans < 1.
!
!    ===================================================================
!    Loops downward over all the layers for each of the NCHN4 channels
!    to compute the layer transmittances TAU.
!
!    The water continuum absorption coefficient is:
!       k_con = the sum i=1 to 5 of { COEF(i)*CONPRD(i) }
!
!    The layer effective fixed gas absorption coefficient is:
!       k_fixed = the sum i=1 to 8 of { COEF(5+i)*FPRED(i) }
!
!    The layer effective CO absorption coefficient is:
!       k_co = the sum i=1 to 9 of { COEF(5+8+i)*OPRED(i) }
!
!    The layer effective ozone absorption coefficient is:
!       k_ozone = the sum i=1 to 3 of { COEF(5+8+9+i)*OPRED(i) }
!
!    The layer effective water lines absorption coefficient is:
!       k_water = the sum i=1 to 11 of { COEF(5+8+9+3+i)*WPRED(i) }
!
!    where
!      "COEF" are the fast transmittance coefficients COEF4
!      "CONPRD" are the water continuum predictors CONPRD
!      "FPRED" are the fixed gases predictors FPRED4
!      "CPRED" are the carbon monoxide predictors CPRED4
!      "OPRED" are the ozone predictors OPRED4
!      "WPRED" are the water lines predictors WPRED4
!
!    The total layer effective transmittance is:
!       TAU = exp( -[ k_con + k_fixed + k_co + k_ozone + k_water])
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
!  1 Dec 1994 Scott Hannon   Created
!  3 Feb 1997 Scott Hannon   Re-wrote (from CALTAU) for FCOW
!  3 Sep 1997 Scott Hannon   Re-wrote for sun and added TAUZ & BLMULT
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
       SUBROUTINE CALT4 ( LTAU, INDCHN, NLAY, BLMULT, NCHN4, CLIST4, &
        COEF4, FIXMUL, CONPD4, FPRED4, CPRED4, OPRED4, WPRED4, TRCPRD, &
        INDCO2, COFCO2, CO2MLT,INDN2O, COFN2O, N2OMLT, XZ, TAU, TAUZ )
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
!
!      Output
       REAL    TAU(MAXLAY,MXCHAN)
       REAL   TAUZ(MXCHAN)


!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
integer ::  I, ICO2, ILAY, IN2O,   J
real(4)     DK, DKCO2, DKN2O, KCO, KCON, KFIX, KLAYER, KOZO, KWAT, KZ
logical ::  LCO2,  LN2O
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
       DO I=1,NCHN4
!
!         Index for TAU
          J=INDCHN( CLIST4(I) )
!
!         Determine whether or not to do variable CO2
          ICO2=INDCO2( CLIST4(I) )
          IF (ICO2 .GT. 0) THEN
             LCO2=.TRUE.
          ELSE
             LCO2=.FALSE.
          ENDIF
!
!         Determine whether or not to do variable N2O
          IN2O=INDN2O( CLIST4(I) )
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
             KCON=( COEF4(1,ILAY,I)*CONPD4(1,ILAY) ) + &
                  ( COEF4(2,ILAY,I)*CONPD4(2,ILAY) ) + &
                  ( COEF4(3,ILAY,I)*CONPD4(3,ILAY) ) + &
                  ( COEF4(4,ILAY,I)*CONPD4(4,ILAY) ) + &
                  ( COEF4(5,ILAY,I)*CONPD4(5,ILAY) ) + &
                  ( COEF4(6,ILAY,I)*CONPD4(6,ILAY) ) + &
                  ( COEF4(7,ILAY,I)*CONPD4(7,ILAY) )
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
             KFIX=( COEF4( 8,ILAY,I)*FPRED4( 1,ILAY) ) + &
                  ( COEF4( 9,ILAY,I)*FPRED4( 2,ILAY) ) + &
                  ( COEF4(10,ILAY,I)*FPRED4( 3,ILAY) ) + &
                  ( COEF4(11,ILAY,I)*FPRED4( 4,ILAY) ) + &
                  ( COEF4(12,ILAY,I)*FPRED4( 5,ILAY) ) + &
                  ( COEF4(13,ILAY,I)*FPRED4( 6,ILAY) ) + &
                  ( COEF4(14,ILAY,I)*FPRED4( 7,ILAY) ) + &
                  ( COEF4(15,ILAY,I)*FPRED4( 8,ILAY) ) + &
                  ( COEF4(16,ILAY,I)*FPRED4( 9,ILAY) ) + &
                  ( COEF4(17,ILAY,I)*FPRED4(10,ILAY) ) + &
                  ( COEF4(18,ILAY,I)*FPRED4(11,ILAY) )
!
             KFIX=KFIX*FIXMUL(ILAY)
!
             IF (KFIX .LT. 0.0E+0) THEN
                KFIX=0.0E+0
             ELSEIF (KFIX .GT. 1.0E+1) THEN
                KFIX=1.0E+1
             ENDIF
!
!            -----------------------
!            Compute the CO abs coef
!            -----------------------
             KCO=( COEF4(19,ILAY,I)*CPRED4( 1,ILAY) ) + &
                 ( COEF4(20,ILAY,I)*CPRED4( 2,ILAY) ) + &
                 ( COEF4(21,ILAY,I)*CPRED4( 3,ILAY) ) + &
                 ( COEF4(22,ILAY,I)*CPRED4( 4,ILAY) ) + &
                 ( COEF4(23,ILAY,I)*CPRED4( 5,ILAY) ) + &
                 ( COEF4(24,ILAY,I)*CPRED4( 6,ILAY) ) + &
                 ( COEF4(25,ILAY,I)*CPRED4( 7,ILAY) ) + &
                 ( COEF4(26,ILAY,I)*CPRED4( 8,ILAY) ) + &
                 ( COEF4(27,ILAY,I)*CPRED4( 9,ILAY) ) + &
                 ( COEF4(28,ILAY,I)*CPRED4(10,ILAY) ) + &
                 ( COEF4(29,ILAY,I)*CPRED4(11,ILAY) )
!
             IF (KCO .LT. 0.0E+0) THEN
                KCO=0.0E+0
             ELSEIF (KCO .GT. 1.0E+1) THEN
                KCO=1.0E+1
             ENDIF
!
!            --------------------------
!            Compute the ozone abs coef
!            --------------------------
             KOZO=( COEF4(30,ILAY,I)*OPRED4(1,ILAY) ) + &
                  ( COEF4(31,ILAY,I)*OPRED4(2,ILAY) ) + &
                  ( COEF4(32,ILAY,I)*OPRED4(3,ILAY) )
!
             IF (KOZO .LT. 0.0E+0) THEN
                KOZO=0.0E+0
             ELSEIF (KOZO .GT. 1.0E+1) THEN
                KOZO=1.0E+1
             ENDIF
!
!            --------------------------
!            Compute the water abs coef
!            --------------------------
             KWAT=( COEF4(33,ILAY,I)*WPRED4( 1,ILAY) ) + &
                  ( COEF4(34,ILAY,I)*WPRED4( 2,ILAY) ) + &
                  ( COEF4(35,ILAY,I)*WPRED4( 3,ILAY) ) + &
                  ( COEF4(36,ILAY,I)*WPRED4( 4,ILAY) ) + &
                  ( COEF4(37,ILAY,I)*WPRED4( 5,ILAY) ) + &
                  ( COEF4(38,ILAY,I)*WPRED4( 6,ILAY) ) + &
                  ( COEF4(39,ILAY,I)*WPRED4( 7,ILAY) ) + &
                  ( COEF4(40,ILAY,I)*WPRED4( 8,ILAY) ) + &
                  ( COEF4(41,ILAY,I)*WPRED4( 9,ILAY) ) + &
                  ( COEF4(42,ILAY,I)*WPRED4(10,ILAY) ) + &
                  ( COEF4(43,ILAY,I)*WPRED4(11,ILAY) ) + &
                  ( COEF4(44,ILAY,I)*WPRED4(12,ILAY) ) + &
                  ( COEF4(45,ILAY,I)*WPRED4(13,ILAY) )
!
             IF (KWAT .LT. 0.0E+0) THEN
                KWAT=0.0E+0
             ELSEIF( KWAT .GT. 1.0E+1) THEN
                KWAT=1.0E+1
             ENDIF
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
!           kco =0.0E+0
!           kozo=0.0E+0
!           kwat=0.0E+0
!cccc
!
!            ----------------------------
!            Calc change in total optical
!            depth due to variable CO2
!            ----------------------------
             IF (LCO2 .AND. CO2MLT(ILAY) .NE. 0) THEN
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
             IF (LN2O .AND. N2OMLT(ILAY) .NE. 0) THEN
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
             KLAYER = KCON + KFIX + KCO + KOZO + KWAT + DK
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
