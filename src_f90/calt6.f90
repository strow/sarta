!=======================================================================
!
!    University of Maryland Baltimore County [UMBC]
!
!    AIRS
!
!    CALT6 (set6=FWO sun mfmw) version for trace gases (CO2 SO2 N2O HDO)
!          (no HNO3, NH3)
!
!F90====================================================================


!ROUTINE NAME:
!    CALT6


!ABSTRACT:
!    Calculate the transmittance for set6 using the predictors and the
!    fast transmittance coefficients.


!CALL PROTOCOL:
!    CALT6( LTAU, INDCHN, NLAY, BMULT, NCHN6, CLIST6, COEF6,
!       FIXMUL, CONPD6, FPRED6, WPRED6, OPRED6, DPRED, TRCPRD,
!       INDCO2, COFCO2, CO2MLT, INDSO2, COFSO2, SO2MLT,
!       INDN2O, COFN2O, N2OMLT, INDHDO, COFHDO, HDOMLT,
!       XZ, TAU, TAUZ )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    LOGICAL   LTAU    Calc all layer trans?       none
!    INT arr   INDCHN  channel indices             none
!    INTEGER   NLAY    number of layers to bottom  none
!    REAL      BLMULT  bottom layer opt depth mult none
!    INTEGER   NCHN6   set6 number of channels     none
!    INT arr   CLIST6  set6 channel list           none
!    REAL arr  COEF6   set6 fast trans coefs       various
!    REAL arr  FIXMUL  fixed amount mult (~1.0)    none
!    REAL arr  CONPD6  set6 H2O continuum preds    various
!    REAL arr  FPRED6  set6 fixed gases preds      various
!    REAL arr  WPRED6  set6 water predictors       various
!    REAL arr  OPRED6  set6 ozone predictors       various
!    REAL arr  DPRED   HDO predictors              various
!    REAL arr  TRCPRD  trace gas pert predictors   various
!    INT arr   INDCO2  CO2 pert chan indices       none
!    REAL arr  COFCO2  CO2 pert coefs              various
!    REAL arr  CO2MLT  CO2 pert multiplier         none
!    REAL arr  SO2PRD  SO2 pert predictors         various
!    INT arr   INDSO2  SO2 pert chan indices       none
!    REAL arr  COFSO2  SO2 pert coefs              various
!    REAL arr  SO2MLT  SO2 pert multiplier         none
!    INT arr   INDN2O  N2O pert chan indices       none
!    REAL arr  COFN2O  N2O pert coefs              various
!    REAL arr  N2OMLT  N2O pert multiplier         none
!    INT arr   INDHDO  HDO pert chan indices       none
!    REAL arr  COFHDO  HDO pert coefs              various
!    REAL arr  HDOMLT  HDO pert multiplier         none
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
!    Loops downward over all the layers for each of the NCHN6 channels
!    to compute the layer transmittances TAU.
!
!    The water continuum absorption coefficient is:
!       k_con = the sum i=1 to 5 of { COEF(i)*CONPRD(i) }
!
!    The layer effective fixed gas absorption coefficient is:
!       k_fixed = the sum i=1 to 8 of { COEF(5+i)*FPRED(i) }
!
!    The layer effective water lines absorption coefficient is:
!       k_water = the sum i=1 to 7 of { COEF(5+8+i)*WPRED(i) }
!
!    The layer effective ozone absorption coefficient is:
!       k_ozone = COEF(5+8+7+1)*OPRED(1)
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
!  3 Jul 1997 Scott Hannon   Created for set6
!  3 Sep 1997 Scott Hannon   Added TAUZ and BLMULT
! 30 Sep 1997 Scott Hannon   Added variable CO2
! 27 Feb 1998 Scott Hannon   Added LTAU
! 11 Aug 2000 Scott Hannon   Change from 4 to 5 term H2O continuum
! 12 Sep 2002 Scott Hannon   Add predictors 6 & 7 to H2O con
!  3 Jan 2003 Scott Hannon   Add XZ
!  6 Feb 2003 Scott Hannon   Bug fix - ozone use coef 23, not 21
! 25 Apr 2003 Scott Hannon   Add SO2
! 28 Jun 2005 Scott Hannon   "trace" version with CO2,SO2,N2O
! 30 Apr 2008 Scott Hannon   Change CO2 from 4 to 5 predictors
! 1  Feb 2019 C Hepplewhite  Add HDO

!END====================================================================

!      =================================================================
       SUBROUTINE CALT6 ( LTAU, INDCHN, NLAY, BLMULT, NCHN6, CLIST6, &
       COEF6, FIXMUL, CONPD6, FPRED6, WPRED6, OPRED6, DPRED, TRCPRD, &
       INDCO2, COFCO2, CO2MLT, INDSO2, COFSO2, SO2MLT, &
       INDN2O, COFN2O, N2OMLT, INDHDO, COFHDO, HDOMLT, &
       XZ, TAU, TAUZ)
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
       INTEGER  NCHN6
       INTEGER CLIST6(MXCHN6)
       REAL  COEF6(N6COEF,MAXLAY,MXCHN6)
       REAL FIXMUL(MAXLAY)
       REAL CONPD6( N6CON,MAXLAY)
       REAL FPRED6( N6FIX,MAXLAY)
       REAL WPRED6( N6H2O,MAXLAY)
       REAL OPRED6(  N6O3,MAXLAY)
       REAL  DPRED(  NHDO,MAXLAY)
       REAL TRCPRD(NTRACE,MAXLAY)
       INTEGER INDCO2(MXCHAN)
       REAL COFCO2(  NCO2,MAXLAY,MXCHNC)
       REAL CO2MLT(MAXLAY)
       INTEGER INDSO2(MXCHAN)
       REAL COFSO2(  NSO2,MAXLAY,MXCHNS)
       REAL SO2MLT(MAXLAY)
       INTEGER INDN2O(MXCHAN)
       REAL COFN2O(  NN2O,MAXLAY,MXCHNN)
       REAL N2OMLT(MAXLAY)
       INTEGER INDHDO(MXCHAN)
       REAL COFHDO(  NHDO,MAXLAY,MXCHND)
       REAL HDOMLT(MAXLAY)
       REAL     XZ
!
!      Output
       REAL    TAU(MAXLAY,MXCHAN)
       REAL   TAUZ(MXCHAN)


!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
integer ::  I, ICO2, ILAY, IN2O, ISO2, IHDO, J
real(4) ::  DK, DKCO2, DKN2O, DKSO2, DKHDO, KHDO, KCON, KFIX, KLAYER
real(4) ::  KOZO, KWAT, KZ
logical ::  LCO2, LN2O, LSO2, LHDO
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
       DO I=1,NCHN6
!
!         Index for TAU
          J=INDCHN( CLIST6(I) )
!
!         Determine whether or not to do variable CO2
          ICO2=INDCO2( CLIST6(I) )
          IF (ICO2 .GT. 0) THEN
             LCO2=.TRUE.
          ELSE
             LCO2=.FALSE.
          ENDIF
!
!         Determine whether or not to do variable SO2
          ISO2=INDSO2( CLIST6(I) )
          IF (ISO2 .GT. 0) THEN
             LSO2=.TRUE.
          ELSE
             LSO2=.FALSE.
          ENDIF
!
!         Determine whether or not to do variable N2O
          IN2O=INDN2O( CLIST6(I) )
          IF (IN2O .GT. 0) THEN
             LN2O=.TRUE.
          ELSE
             LN2O=.FALSE.
          ENDIF
!
!         Determine whether or not to do variable HDO calc
          IHDO=INDHDO( CLIST6(I) )
          IF (IHDO .GT. 0) THEN
             LHDO=.TRUE.
          ELSE
             LHDO=.FALSE.
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
             KCON=( COEF6(1,ILAY,I)*CONPD6(1,ILAY) ) + &
                  ( COEF6(2,ILAY,I)*CONPD6(2,ILAY) ) + &
                  ( COEF6(3,ILAY,I)*CONPD6(3,ILAY) ) + &
                  ( COEF6(4,ILAY,I)*CONPD6(4,ILAY) ) + &
                  ( COEF6(5,ILAY,I)*CONPD6(5,ILAY) ) + &
                  ( COEF6(6,ILAY,I)*CONPD6(6,ILAY) ) + &
                  ( COEF6(7,ILAY,I)*CONPD6(7,ILAY) )
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
             KFIX=( COEF6( 8,ILAY,I)*FPRED6( 1,ILAY) ) + &
                  ( COEF6( 9,ILAY,I)*FPRED6( 2,ILAY) ) + &
                  ( COEF6(10,ILAY,I)*FPRED6( 3,ILAY) ) + &
                  ( COEF6(11,ILAY,I)*FPRED6( 4,ILAY) ) + &
                  ( COEF6(12,ILAY,I)*FPRED6( 5,ILAY) ) + &
                  ( COEF6(13,ILAY,I)*FPRED6( 6,ILAY) ) + &
                  ( COEF6(14,ILAY,I)*FPRED6( 7,ILAY) ) + &
                  ( COEF6(15,ILAY,I)*FPRED6( 8,ILAY) )
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
             KWAT=( COEF6(16,ILAY,I)*WPRED6( 1,ILAY) ) + &
                  ( COEF6(17,ILAY,I)*WPRED6( 2,ILAY) ) + &
                  ( COEF6(18,ILAY,I)*WPRED6( 3,ILAY) ) + &
                  ( COEF6(19,ILAY,I)*WPRED6( 4,ILAY) ) + &
                  ( COEF6(20,ILAY,I)*WPRED6( 5,ILAY) ) + &
                  ( COEF6(21,ILAY,I)*WPRED6( 6,ILAY) ) + &
                  ( COEF6(22,ILAY,I)*WPRED6( 7,ILAY) )
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
             KOZO=( COEF6(23,ILAY,I)*OPRED6(1,ILAY) )
!
             IF (KOZO .LT. 0.0E+0) THEN
                KOZO=0.0E+0
             ELSEIF (KOZO .GT. 1.0E+1) THEN
                KOZO=1.0E+1
             ENDIF
!
!            --------------------------
!            Compute the HDO abs coef
!            --------------------------
             IF (LHDO) THEN
                KHDO=( COFHDO(1,ILAY,IHDO)*DPRED( 1,ILAY) ) + &
                     ( COFHDO(2,ILAY,IHDO)*DPRED( 2,ILAY) ) + &
                     ( COFHDO(3,ILAY,IHDO)*DPRED( 3,ILAY) ) + &
                     ( COFHDO(4,ILAY,IHDO)*DPRED( 4,ILAY) ) + &
                     ( COFHDO(5,ILAY,IHDO)*DPRED( 5,ILAY) ) + &
                     ( COFHDO(6,ILAY,IHDO)*DPRED( 6,ILAY) ) + &
                     ( COFHDO(7,ILAY,IHDO)*DPRED( 7,ILAY) ) + &
                     ( COFHDO(8,ILAY,IHDO)*DPRED( 8,ILAY) ) + &
                     ( COFHDO(9,ILAY,IHDO)*DPRED( 9,ILAY) ) + &
                     ( COFHDO(10,ILAY,IHDO)*DPRED(10,ILAY) ) + &
                     ( COFHDO(11,ILAY,IHDO)*DPRED(11,ILAY) )
!
!              	 IF (KHDO .LT. 0.0E+0) KHDO=0.0E+0
                KHDO=KHDO*HDOMLT(ILAY)
             ELSE
               	KHDO=0.0
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
!            depth due to variable SO2
!            ----------------------------
             IF (LSO2 .AND. SO2MLT(ILAY) .NE. 0.0) THEN
                DKSO2=( COFSO2(1,ILAY,ISO2)*TRCPRD(1,ILAY) ) + &
                      ( COFSO2(2,ILAY,ISO2)*TRCPRD(2,ILAY) ) + &
                      ( COFSO2(3,ILAY,ISO2)*TRCPRD(3,ILAY) ) + &
                      ( COFSO2(4,ILAY,ISO2)*TRCPRD(4,ILAY) )
                DKSO2=DKSO2*SO2MLT(ILAY)
             ELSE
                DKSO2=0.0
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
!            ----------------------------
!            Calc change in total optical
!            depth due to variable HDO
!            ----------------------------
!             IF (LHDO .AND. HDOMLT(ILAY) .NE. 0) THEN
!                DKHDO=( COFHDO(1,ILAY,IHDO)*TRCPRD(1,ILAY) ) +
!     $                ( COFHDO(2,ILAY,IHDO)*TRCPRD(2,ILAY) ) +
!     $                ( COFHDO(3,ILAY,IHDO)*TRCPRD(3,ILAY) ) +
!     $                ( COFHDO(4,ILAY,IHDO)*TRCPRD(4,ILAY) )
!                DKHDO=DKHDO*HDOMLT(ILAY)
!             ELSE
!                DKHDO=0.0
!             ENDIF
!
!cc
! this block for testing
!       DKCO2=0.0
!       DKSO2=0.0
!       DKN2O=0.0
!       DKHDO=0.0
!       KHDO=0.0
       IF (.NOT. CFHDO) KHDO=0.0
!cc
!            Limit -DK so it can never totally totally cancel KFIX
             DK = DKCO2 + DKSO2 + DKN2O
             IF (-DK .GE. KFIX) THEN
                DK = -0.999*KFIX
             ENDIF

!            Calc total layer optical depth
             KLAYER = KCON + KFIX + KWAT + KOZO + KHDO + DK
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
