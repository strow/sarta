!=======================================================================
!
!    University of Maryland Baltimore County [UMBC]
!
!    AIRS
!
!    CALT2 (for set2 = FOW) version with NH3, HDO
!
!F90====================================================================


!ROUTINE NAME:
!    CALT2


!ABSTRACT:
!    Calculate the transmittance for set2 using the predictors and the
!    fast transmittance coefficients.


!CALL PROTOCOL:
!    CALT2 ( INDCHN, NLAY, BLMULT, NCHN2, CLIST2, COEF2, FIXMUL,
!       CONPD2, FPRED2, OPRED2, WPRED2, DPRED,  TRCPRD,
!       INDCO2, COFCO2, CO2MLT, INDSO2, SOFCO2, SO2MLT,
!       INDHNO, COFHNO, HNOMLT, INDN2O, COFN2O, N2OMLT,
!       INDNH3, COFNH3, NH3MLT, INDHDO, COFHDO, HDOMLT,
!       TAU, TAUZ )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INT arr   INDCHN  channel indices             none
!    INTEGER   NLAY    number of layers to bottom  none
!    REAL      BLMULT  bottom layer opt depth mult none
!    INTEGER   NCHN2   set2 number of channels     none
!    INT arr   CLIST2  set2 channel list           none
!    REAL arr  COEF2   set2 fast trans coefs       various
!    REAL arr  FIXMUL  fixed amount mult (~1.0)    none
!    REAL arr  CONPD2  set2 H2O continuum preds    various
!    REAL arr  FPRED2  set2 fixed gases preds      various
!    REAL arr  OPRED2  set2 ozone predictors       various
!    REAL arr  WPRED2  set2 water predictors       various
!    REAL arr  DPRED   HDO predictors              various
!    REAL arr  TRCPRD  Trace gas pert predictors   various
!    INT arr   INDCO2  CO2 pert chan indices       none
!    REAL arr  COFCO2  CO2 pert coefs              various
!    REAL arr  CO2MLT  CO2 pert multiplier         none
!    INT arr   INDSO2  SO2 pert chan indices       none
!    REAL arr  COFSO2  SO2 pert coefs              various
!    REAL arr  SO2MLT  SO2 pert multiplier         none
!    INT arr   INDHNO  HNO3 pert chan indices      none
!    REAL arr  COFHNO  HNO3 pert coefs             various
!    REAL arr  HNOMLT  HNO3 pert multiplier        none
!    INT arr   INDN2O  N2O pert chan indices       none
!    REAL arr  COFN2O  N2O pert coefs              various
!    REAL arr  N2OMLT  N2O pert multiplier         none
!    INT arr   INDNH3  NH3 pert chan indices       none
!    REAL arr  COFNH3  NH3 pert coefs              various
!    REAL arr  NH3MLT  NH3 pert multiplier         none
!    INT arr   INDHDO  HDO pert chan indices       none
!    REAL arr  COFHDO  HDO pert coefs              various
!    REAL arr  HDOMLT  HDO pert multiplier         none


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
!    transmittances. Fixed, ozone, and water transmittances are each
!    checked individually to be sure they give 0 < trans < 1.
!
!    ===================================================================
!    Loops downward over all the layers for each of the NCHN2 channels
!    to compute the layer transmittances TAU.
!
!    The water continuum absorption coefficient is:
!       k_con = the sum i=1 to 5 of { COEF(i)*CONPRD(i) }
!
!    The layer effective fixed gas absorption coefficient is:
!       k_fixed = the sum i=1 to 8 of { COEF(5+i)*FPRED(i) }
!
!    The layer effective ozone absorption coefficient is:
!       k_ozone = the sum i=1 to 10 of { COEF(5+8+i)*OPRED(i) }
!
!    The layer effective water lines absorption coefficient is:
!       k_water = the sum i=1 to 11 of { COEF(5+8+10+i)*WPRED(i) }
!
!    where
!      "COEF" are the fast transmittance coefficients COEF2
!      "CONPRD" are the water continuum predictors CONPRD
!      "FPRED" are the fixed gases predictors FPRED2
!      "OPRED" are the ozone predictors OPRED2
!      "WPRED" are the water lines predictors WPRED2
!
!    The total layer effective transmittance TAU is:
!       TAU = exp( -[ k_con + k_fixed + k_ozone + k_water])
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
!    Date        Programmer     Comments
!    ----------- -------------- ----------------------------------------
!    Dec  1 1994 Scott Hannon   Created
!     3 Feb 1997 Scott Hannon   Re-wrote (from CALTAU) for FOW
!     3 Sep 1997 Scott Hannon   Added TAUZ and BLMULT
!    30 Sep 1997 Scott Hannon   Added variable CO2
!    11 Aug 2000 Scott Hannon   Change from 4 to 5 term H2O continuum
!    12 Sep 2002 Scott Hannon   Add predictors 6 & 7 to H2O con
!    18 May 2005 Scott Hannon   Add HNO3 based on SO2 code
!    28 Jun 2005 Scott Hannon   "trace" version for CO2,SO2,HNO3,N2O.
!    13 Sep 2010 Scott Hannon   Add 5th CO2 coef
!    10 May 2018 C Hepplewhite  Add NH3
!    1  Feb 2019 C Hepplewhite  Add HDO

!END====================================================================

!      =================================================================
       SUBROUTINE CALT2 ( INDCHN, NLAY, BLMULT, NCHN2, CLIST2, COEF2, &
         FIXMUL, CONPD2, FPRED2, OPRED2, WPRED2, DPRED,  TRCPRD, &
         INDCO2, COFCO2, CO2MLT, INDSO2, COFSO2, SO2MLT, &
         INDHNO, COFHNO, HNOMLT, INDN2O, COFN2O, N2OMLT, &
         INDNH3, COFNH3, NH3MLT, INDHDO, COFHDO, HDOMLT, TAU, TAUZ )

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
       INTEGER INDCHN(MXCHAN)
       INTEGER   NLAY
       REAL BLMULT
       INTEGER  NCHN2
       INTEGER CLIST2(MXCHN2)
       REAL  COEF2(N2COEF,MAXLAY,MXCHN2)
       REAL FIXMUL(MAXLAY)
       REAL CONPD2( N2CON,MAXLAY)
       REAL FPRED2( N2FIX,MAXLAY)
       REAL OPRED2(  N2O3,MAXLAY)
       REAL WPRED2( N2H2O,MAXLAY)
       REAL DPRED(   NHDO,MAXLAY)
       REAL TRCPRD(NTRACE,MAXLAY)
       INTEGER INDCO2(MXCHAN)
       REAL COFCO2(  NCO2,MAXLAY,MXCHNC)
       REAL CO2MLT(MAXLAY)
       INTEGER INDSO2(MXCHAN)
       REAL COFSO2(  NSO2,MAXLAY,MXCHNS)
       REAL SO2MLT(MAXLAY)
       INTEGER INDHNO(MXCHAN)
       REAL COFHNO( NHNO3,MAXLAY,MXCHNH)
       REAL HNOMLT(MAXLAY)
       INTEGER INDN2O(MXCHAN)
       REAL COFN2O(  NN2O,MAXLAY,MXCHNN)
       REAL N2OMLT(MAXLAY)
       INTEGER INDNH3(MXCHAN)
       REAL COFNH3(  NNH3,MAXLAY,MXCHNA)
       REAL NH3MLT(MAXLAY)
       INTEGER INDHDO(MXCHAN)
       REAL COFHDO(  NHDO,MAXLAY,MXCHND)
       REAL HDOMLT(MAXLAY)
!
!      Output
       REAL    TAU(MAXLAY,MXCHAN)
       REAL   TAUZ(MXCHAN)

!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
integer :: I, ICO2, IHNO3, ILAY, IN2O, INH3, ISO2, IHDO, J
real(4) :: DK, DKCO2, DKHNO3, DKN2O, DKNH3, DKSO2, DKHDO, KHDO
real(4) :: KCON, KFIX, KLAYER, KOZO, KWAT,  KZ
logical :: LCO2, LHNO3, LN2O, LNH3,  LSO2,  LHDO
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
       DO I=1,NCHN2
!
!         Index for TAU
          J=INDCHN( CLIST2(I) )
!
!         Determine whether or not to do variable CO2
          ICO2=INDCO2( CLIST2(I) )
          IF (ICO2 .GT. 0) THEN
             LCO2=.TRUE.
          ELSE
             LCO2=.FALSE.
          ENDIF
!
!         Determine whether or not to do variable CO2
          ISO2=INDSO2( CLIST2(I) )
          IF (ISO2 .GT. 0) THEN
             LSO2=.TRUE.
          ELSE
             LSO2=.FALSE.
          ENDIF
!
!         Determine whether or not to do variable HNO3
          IHNO3=INDHNO( CLIST2(I) )
          IF (IHNO3 .GT. 0) THEN
             LHNO3=.TRUE.
          ELSE
             LHNO3=.FALSE.
          ENDIF
!
!         Determine whether or not to do variable N2O
          IN2O=INDN2O( CLIST2(I) )
          IF (IN2O .GT. 0) THEN
             LN2O=.TRUE.
          ELSE
             LN2O=.FALSE.
          ENDIF
!
!         Determine whether or not to do variable NH3
          INH3=INDNH3( CLIST2(I) )
          IF (INH3 .GT. 0) THEN
             LNH3=.TRUE.
          ELSE
             LNH3=.FALSE.
          ENDIF
!
!         Determine whether or not to do variable HDO calc
          IHDO=INDHDO( CLIST2(I) )
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

!            ---------------------------
!            Compute the water continuum
!            ---------------------------
             KCON=( COEF2(1,ILAY,I)*CONPD2(1,ILAY) ) + &
                  ( COEF2(2,ILAY,I)*CONPD2(2,ILAY) ) + &
                  ( COEF2(3,ILAY,I)*CONPD2(3,ILAY) ) + &
                  ( COEF2(4,ILAY,I)*CONPD2(4,ILAY) ) + &
                  ( COEF2(5,ILAY,I)*CONPD2(5,ILAY) ) + &
                  ( COEF2(6,ILAY,I)*CONPD2(6,ILAY) ) + &
                  ( COEF2(7,ILAY,I)*CONPD2(7,ILAY) )
!
             IF (KCON .LT. 0.0+0) THEN
                KCON=0.0E+0
             ELSEIF (KCON .GT. 1.0E+1) THEN
                KCON=1.0E+1
             ENDIF
!

!            -----------------------------
!            Calc the fixed gases abs coef
!            -----------------------------
             KFIX=( COEF2( 8,ILAY,I)*FPRED2(1,ILAY) ) + &
                  ( COEF2( 9,ILAY,I)*FPRED2(2,ILAY) ) + &
                  ( COEF2(10,ILAY,I)*FPRED2(3,ILAY) ) + &
                  ( COEF2(11,ILAY,I)*FPRED2(4,ILAY) ) + &
                  ( COEF2(12,ILAY,I)*FPRED2(5,ILAY) ) + &
                  ( COEF2(13,ILAY,I)*FPRED2(6,ILAY) ) + &
                  ( COEF2(14,ILAY,I)*FPRED2(7,ILAY) ) + &
                  ( COEF2(15,ILAY,I)*FPRED2(8,ILAY) )
!
             KFIX=KFIX*FIXMUL(ILAY)
!
             IF (KFIX .LT. 0.0E+0) THEN
                KFIX=0.0E+0
             ELSEIF (KFIX .GT. 1.0E+1) THEN
                KFIX=1.0E+1
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
!                IF (KHDO .LT. 0.0E+0) KHDO=0.0E+0
                KHDO=KHDO*HDOMLT(ILAY)
             ELSE
                KHDO=0.0
             ENDIF
!
!            --------------------------
!            Compute the ozone abs coef
!            --------------------------
             KOZO=( COEF2(16,ILAY,I)*OPRED2( 1,ILAY) ) + &
                  ( COEF2(17,ILAY,I)*OPRED2( 2,ILAY) ) + &
                  ( COEF2(18,ILAY,I)*OPRED2( 3,ILAY) ) + &
                  ( COEF2(19,ILAY,I)*OPRED2( 4,ILAY) ) + &
                  ( COEF2(20,ILAY,I)*OPRED2( 5,ILAY) ) + &
                  ( COEF2(21,ILAY,I)*OPRED2( 6,ILAY) ) + &
                  ( COEF2(22,ILAY,I)*OPRED2( 7,ILAY) ) + &
                  ( COEF2(23,ILAY,I)*OPRED2( 8,ILAY) ) + &
                  ( COEF2(24,ILAY,I)*OPRED2( 9,ILAY) ) + &
                  ( COEF2(25,ILAY,I)*OPRED2(10,ILAY) )
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
             KWAT=( COEF2(26,ILAY,I)*WPRED2( 1,ILAY) ) + &
                  ( COEF2(27,ILAY,I)*WPRED2( 2,ILAY) ) + &
                  ( COEF2(28,ILAY,I)*WPRED2( 3,ILAY) ) + &
                  ( COEF2(29,ILAY,I)*WPRED2( 4,ILAY) ) + &
                  ( COEF2(30,ILAY,I)*WPRED2( 5,ILAY) ) + &
                  ( COEF2(31,ILAY,I)*WPRED2( 6,ILAY) ) + &
                  ( COEF2(32,ILAY,I)*WPRED2( 7,ILAY) ) + &
                  ( COEF2(33,ILAY,I)*WPRED2( 8,ILAY) ) + &
                  ( COEF2(34,ILAY,I)*WPRED2( 9,ILAY) ) + &
                  ( COEF2(35,ILAY,I)*WPRED2(10,ILAY) ) + &
                  ( COEF2(36,ILAY,I)*WPRED2(11,ILAY) )
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
!            depth due to variable SO2
!            ----------------------------
             IF (LSO2 .AND. SO2MLT(ILAY) .NE. 0) THEN
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
!            depth due to variable HNO3
!            ----------------------------
             IF (LHNO3 .AND. HNOMLT(ILAY) .NE. 0) THEN
                DKHNO3=( COFHNO(1,ILAY,IHNO3)*TRCPRD(1,ILAY) ) + &
                       ( COFHNO(2,ILAY,IHNO3)*TRCPRD(2,ILAY) ) + &
                       ( COFHNO(3,ILAY,IHNO3)*TRCPRD(3,ILAY) ) + &
                       ( COFHNO(4,ILAY,IHNO3)*TRCPRD(4,ILAY) )
                DKHNO3=DKHNO3*HNOMLT(ILAY)
             ELSE
                DKHNO3=0.0
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
!            ----------------------------
!            Calc change in total optical
!            depth due to variable NH3
!            ----------------------------
             IF (LNH3 .AND. NH3MLT(ILAY) .NE. 0) THEN
                DKNH3=( COFNH3(1,ILAY,INH3)*TRCPRD(1,ILAY) ) + & 
                      ( COFNH3(2,ILAY,INH3)*TRCPRD(2,ILAY) ) + &
                      ( COFNH3(3,ILAY,INH3)*TRCPRD(3,ILAY) ) + &
                      ( COFNH3(4,ILAY,INH3)*TRCPRD(4,ILAY) )
                DKNH3=DKNH3*NH3MLT(ILAY)
             ELSE
                DKNH3=0.0
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

!            ------------------------------------------
!            Calc total optical depth and transmittance
!            ------------------------------------------
!            Calc total layer optical depth
!cc
! this block for testing
!       DKCO2=0.0
!       DKSO2=0.0
!       DKHNO3=0.0
!       DKN2O=0.0
!       DKNH3=0.0
!       DKHDO=0.0
!       KHDO=0.0
       IF (.NOT. CFHDO) KHDO=0.0
!cc
!            Limit -DK so it can never totally totally cancel KFIX
             DK = DKCO2 + DKSO2 + DKHNO3 + DKN2O + DKNH3
             IF (-DK .GE. KFIX) THEN
                DK = -0.999*KFIX
             ENDIF

             KLAYER = KCON + KFIX + KOZO + KWAT + KHDO + DK
!
!            Adjust the optical depth of the bottom layer
             IF (ILAY .EQ. NLAY) KLAYER=BLMULT*KLAYER
!
!            Calc layer-to-space optical depth
             KZ=KZ + KLAYER
!
!            Calc effective layer transmittance
             TAU(ILAY,J)=QIKEXP(-KLAYER)
!
          ENDDO
!         End loop on levels
!
!         Convert KZ to TAUZ
          TAUZ(J)=QIKEXP(-KZ)
!
       ENDDO
!      End loops on channel number (frequency)
!
       RETURN
       END
