!=======================================================================
!
!    University of Maryland Baltimore County [UMBC]
!
!    AIRS
!
!    CALT1 (for set1 = FWO)  version with NH3, HDO
!    
!
!F90====================================================================


!ROUTINE NAME:
!    CALT1


!ABSTRACT:
!    Calculate the transmittance for set1 using the predictors
!    and the fast transmittance coefficients.


!CALL PROTOCOL:
!    CALT1 ( INDCHN, NLAY, BLMULT, NCHN1, CLIST1, COEF1,
!      FIXMUL, CONPD1, FPRED1, WPRED1, DPRED,  OPRED1, TRCPRD,
!      INDCO2, COFCO2, CO2MLT, INDSO2, COFSO2, SO2MLT,
!      INDHNO, COFHNO, HNOMLT, INDN2O, COFN2O, N2OMLT,
!      INDNH3, COFNH3, NH3MLT, INDHDO, COFHDO, HDOMLT,
!      INDH2O, H2OPRD, COFH2O, LOPMIN, LOPMAX,
!      LOPLOW, LOPUSE, WAOP, DAOP, WAANG, TAU, TAUZ)


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL      BLMULT  bottom layer opt depth mult none
!    INT arr   CLIST1  set channel list            none
!    REAL arr  COEF1   set1 fast trans coefs       various
!    REAL arr  CONPD1  set1 H2O continuum preds    various
!    REAL arr  FIXMUL  fixed amount mult (~1.0)    none
!    REAL arr  FPRED1  set1 fixed gases preds      various
!    INT arr   INDCHN  channel indices             none
!    INTEGER   NLAY    Number of layers to bottom  none
!    INTEGER   NCHN1   set1 number of channels     none
!    REAL arr  OPRED1  set1 ozone predictors       various
!    REAL arr  WPRED1  set1 water predictors       various
!    REAL arr  DPRED   HDO predictors              various
!    REAL arr  TRCPRD  trace gas pert predictors   various
!    INT arr   INDCO2  CO2 pert chan indices       none
!    REAL arr  COFCO2  CO2 pert coefs              various
!    REAL arr  CO2MLT  CO2 pert multiplier         none
!    INT arr   INDSO2  SO2 pert chan indices       none
!    REAL arr  COFSO2  SO2 pert coefs              various
!    REAL      SO2MLT  SO2 pert multiplier         none
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
!    INT arr   INDH2O  OPTRAN H2O chan indices     none
!    REAL arr  H2OPRD  OPTRAN H2O predictors       various
!    REAL arr  COFH2O  OPTRAN H2O coefs            various
!    INTEGER   LOPMAX  OPTRAN max level            none
!    INTEGER   LOPLOW  OPTRAN low bracketing level none
!    LOG arr   LOPUSE  OPTRAN level needed?        none
!    REAL arr  WAOP    OPTRAN layer water amounts  kilomoles/cm^2
!    REAL arr  DAOP    OPTRAN-to-AIRS interp fac   none
!    REAL arr  WAANG   AIRS layer water amounts    kilomoles/cm^2


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
!    The FTC coefficents and profile FTC variables are multiplied
!    together and summed to calculate the effective layer
!    transmittances. Fixed, water, and ozone transmittances are each
!    checked individually to be sure they give 0 < trans < 1.
!
!    ===================================================================
!    The routine loops over the selected channels of set1.  For each
!    channel, it first decides if needs to do a calculation for
!    variable CO2, and also if it needs to do an OPTRAN water calc (if
!    so, it does so immediately).  The program then loops downward over
!    all the layers and computes the layer transmittances TAU.
!
!    The water continuum absorption coefficient is:
!       k_con = the sum i=1 to 5 of { COEF(i)*CONPRD(i) }
!
!    The layer effective fixed gas absorption coefficient is:
!       k_fixed = the sum i=1 to 8 of { COEF(5+i)*FPRED(i) }
!
!    The layer effective water lines absorption coefficient is:
!       k_water = the sum i=1 to 11 of { COEF(5+8+i)*WPRED(i) }
!
!    The layer effective ozone absorption coefficient is:
!       k_ozone = the sum i=1 to 5 of { COEF(5+8+11+i)*OPRED(i) }
!
!    where
!      "COEF" are the fast transmittance coefficients COEF1
!      "CONPRD" are the water continuum predictors CONPRD
!      "FPRED" are the fixed gases predictors FPRED1
!      "WPRED" are the water lines predictors WPRED1
!      "OPRED" are the ozone predictors OPRED1
!
!    The total layer effective transmittance TAU is:
!       TAU = exp( -[ k_con + k_fixed + k_water + k_ozone ])
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
!     3 Feb 1997 Scott Hannon   Re-wrote (from CALTAU) for FWO
!     3 Sep 1997 Scott Hannon   Added TAUZ and BLMULT
!    30 Sep 1997 Scott Hannon   Added variable CO2
!    27 Feb 1998 Scott Hannon   Added OPTRAN H2O
!     6 Mar 1998 Scott Hannon   Deleted water preds 12 & 13 and shifted
!                               ozone coef indices to 24-28 (was 26-30)
!     4 May 1998 Scott Hannon   Fix error: INDH2O(MXCHAN) not (MXCHNW)
!    26 Aug 1998 Scott Hannon   Add NLAY to call to CALOKW
!    11 Aug 2000 Scott Hannon   Change from 4 to 5 term H2O continuum
!    12 Sep 2002 Scott Hannon   Add predictors 6 & 7 to H2O con
!    18 May 2005 Scott Hannon   Add HNO3 based on SO2 code
!    28 Jun 2005 Scott Hannon   "trace" version for CO2,SO2,HNO3,N2O
!    13 Sep 2010 Scott Hannon   Add 5th CO2 coef
!    10 May 2018 C Hepplewhite  Add NH3
!    1  Feb 2019 C Hepplewhite  Add HDO

!END====================================================================

!      =================================================================
       SUBROUTINE CALT1 ( INDCHN, NLAY, BLMULT, NCHN1, CLIST1, COEF1, &
         FIXMUL, CONPD1, FPRED1, WPRED1, DPRED,  OPRED1, TRCPRD, &
         INDCO2, COFCO2, CO2MLT, INDSO2, COFSO2, SO2MLT, &
         INDHNO, COFHNO, HNOMLT, INDN2O, COFN2O, N2OMLT, &
         INDNH3, COFNH3, NH3MLT, INDHDO, COFHDO, HDOMLT, &
         INDH2O, H2OPRD, COFH2O, LOPMIN, LOPMAX, LOPLOW, LOPUSE, &
           WAOP,   DAOP,  WAANG,    TAU,   TAUZ)
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
       INTEGER  NCHN1
       INTEGER CLIST1(MXCHN1)
       REAL  COEF1(N1COEF,MAXLAY,MXCHN1)
       REAL FIXMUL(MAXLAY)
       REAL CONPD1( N1CON,MAXLAY)
       REAL FPRED1( N1FIX,MAXLAY)
       REAL WPRED1( N1H2O,MAXLAY)
       REAL DPRED(   NHDO,MAXLAY)
       REAL OPRED1(  N1O3,MAXLAY)
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
       INTEGER INDH2O(MXCHAN)
       REAL H2OPRD(  NH2O,MXOWLY)
       REAL COFH2O(  NH2O,MXOWLY,MXCHNW)
       INTEGER LOPMIN
       INTEGER LOPMAX
       INTEGER LOPLOW(MAXLAY)
       LOGICAL LOPUSE(MXOWLY)
       REAL   WAOP(MXOWLY)
       REAL   DAOP(MAXLAY)
       REAL  WAANG(MAXLAY)
!
!      Output
       REAL   TAU(MAXLAY,MXCHAN)
       REAL   TAUZ(MXCHAN)


!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
integer :: I, ICO2, IHNO3, ILAY, IN2O, INH3, IHDO, ISO2, J
real(4) :: DK, DKCO2, DKHNO3, DKN2O, DKSO2, DKNH3, DKHDO, KHDO
real(4) :: KCON, KFIX, KLAYER, KOZO, KZ, KZFW
logical :: LCO2, LH2O, LHNO3, LN2O, LNH3, LHDO, LSO2
!      for function QIKEXP
REAL QIKEXP
!
!      for CALOKW
integer ::  IH2O
real, dimension(MAXLAY) :: KW


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
       DO I=1,NCHN1
!
!         Array index of channel in TAU
          J=INDCHN( CLIST1(I) )
!
!         Determine whether or not to do variable CO2 calc
          ICO2=INDCO2( CLIST1(I) )
          IF (ICO2 .GT. 0) THEN
             LCO2=.TRUE.
          ELSE
             LCO2=.FALSE.
          ENDIF
!
!         Determine whether or not to do variable SO2 calc
          ISO2=INDSO2( CLIST1(I) )
          IF (ISO2 .GT. 0) THEN
             LSO2=.TRUE.
          ELSE
             LSO2=.FALSE.
          ENDIF
!
!         Determine whether or not to do variable HNO3 calc
          IHNO3=INDHNO( CLIST1(I) )
          IF (IHNO3 .GT. 0) THEN
             LHNO3=.TRUE.
          ELSE
             LHNO3=.FALSE.
          ENDIF
!
!         Determine whether or not to do variable N2O calc
          IN2O=INDN2O( CLIST1(I) )
          IF (IN2O .GT. 0) THEN
             LN2O=.TRUE.
          ELSE
             LN2O=.FALSE.
          ENDIF
!
!         Determine whether or not to do variable NH3 calc
          INH3=INDNH3( CLIST1(I) )
          IF (INH3 .GT. 0) THEN
             LNH3=.TRUE.
          ELSE
             LNH3=.FALSE.
          ENDIF
!
!         Determine whether or not to do variable HDO calc
          IHDO=INDHDO( CLIST1(I) )
          IF (IHDO .GT. 0) THEN
             LHDO=.TRUE.
          ELSE
             LHDO=.FALSE.
          ENDIF
!          write(6,'(A,3(X,I4),X,L5)') 'calt1: I,CLIST1(I),IHDO = ', I,CLIST1(I),IHDO,LHDO
!
!         -------------------------
!         Do OPTRAN water if needed
!         -------------------------
          IH2O=INDH2O( CLIST1(I) )
          IF (IH2O .GT. 0) THEN
             LH2O=.FALSE.
!            Calc OPTRAN water
!
             CALL CALOKW( NLAY, IH2O, LOPMIN, LOPMAX, LOPLOW, LOPUSE, &
               H2OPRD, COFH2O, WAOP, DAOP, WAANG, KW )
!
          ELSE
             LH2O=.TRUE.
          ENDIF
!
!         Initialize the layer-to-space optical depth
          KZ=0.0E+0
          KZFW=0.0E+0
!
!         ------------------------------
!         Loop on layers (top to bottom)
!         ------------------------------
!          write(6,'(A,X,I4)') 'calt1: NLAY= ', NLAY
          DO ILAY=1,NLAY

!            ---------------------------
!            Compute the water continuum
!            ---------------------------
             KCON=( COEF1(1,ILAY,I)*CONPD1(1,ILAY) ) + &
                 ( COEF1(2,ILAY,I)*CONPD1(2,ILAY) ) + &
                 ( COEF1(3,ILAY,I)*CONPD1(3,ILAY) ) + &
                 ( COEF1(4,ILAY,I)*CONPD1(4,ILAY) ) + &
                 ( COEF1(5,ILAY,I)*CONPD1(5,ILAY) ) + &
                 ( COEF1(6,ILAY,I)*CONPD1(6,ILAY) ) + &
                 ( COEF1(7,ILAY,I)*CONPD1(7,ILAY) )
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
             KFIX=( COEF1( 8,ILAY,I)*FPRED1(1,ILAY) ) + &
                 ( COEF1( 9,ILAY,I)*FPRED1(2,ILAY) ) + &
                 ( COEF1(10,ILAY,I)*FPRED1(3,ILAY) ) + &
                 ( COEF1(11,ILAY,I)*FPRED1(4,ILAY) ) + &
                 ( COEF1(12,ILAY,I)*FPRED1(5,ILAY) ) + &
                 ( COEF1(13,ILAY,I)*FPRED1(6,ILAY) ) + &
                 ( COEF1(14,ILAY,I)*FPRED1(7,ILAY) ) + &
                 ( COEF1(15,ILAY,I)*FPRED1(8,ILAY) )
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
!            Compute the water abs coef
!            --------------------------
             IF (LH2O) THEN
!               Not an OPTRAN water channel
                KW(ILAY)= &
                    ( COEF1(16,ILAY,I)*WPRED1( 1,ILAY) ) + &
                    ( COEF1(17,ILAY,I)*WPRED1( 2,ILAY) ) + &
                    ( COEF1(18,ILAY,I)*WPRED1( 3,ILAY) ) + &
                    ( COEF1(19,ILAY,I)*WPRED1( 4,ILAY) ) + &
                    ( COEF1(20,ILAY,I)*WPRED1( 5,ILAY) ) + &
                    ( COEF1(21,ILAY,I)*WPRED1( 6,ILAY) ) + &
                    ( COEF1(22,ILAY,I)*WPRED1( 7,ILAY) ) + &
                    ( COEF1(23,ILAY,I)*WPRED1( 8,ILAY) ) + &
                    ( COEF1(24,ILAY,I)*WPRED1( 9,ILAY) ) + &
                    ( COEF1(25,ILAY,I)*WPRED1(10,ILAY) ) + &
                    ( COEF1(26,ILAY,I)*WPRED1(11,ILAY) )
!
                IF (KW(ILAY) .LT. 0.0E+0) KW(ILAY)=0.0E+0
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
                KHDO = KHDO*HDOMLT(ILAY)
             ELSE
                KHDO=0.0
             ENDIF
!

!            --------------------------
!            Compute the ozone abs coef
!            --------------------------
             KOZO=( COEF1(27,ILAY,I)*OPRED1(1,ILAY) ) + &
                  ( COEF1(28,ILAY,I)*OPRED1(2,ILAY) ) + &
                  ( COEF1(29,ILAY,I)*OPRED1(3,ILAY) ) + &
                  ( COEF1(30,ILAY,I)*OPRED1(4,ILAY) ) + &
                  ( COEF1(31,ILAY,I)*OPRED1(5,ILAY) )
!
             IF (KOZO .LT. 0.0E+0) THEN
                KOZO=0.0E+0
             ELSEIF (KOZO .GT. 1.0E+1) THEN
                KOZO=1.0E+1
             ENDIF
!
!            Update KZFW
             KZFW=KZFW + KFIX + KW(ILAY)
!
!            ----------------------------------
!            Calc the total layer transmittance
!            ----------------------------------
!
!cccc
! This block is usually commented out and is only uncommented for
! testing purposes.
!c
!           kcon=0.0E+0
!           kfix=0.0E+0
!           kw(ilay)=0.0E+0
!           kozo=0.0E+0
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
!      DKHNO3=0.0
!      DKSO2=0.0
!      DKCO2=0.0
!      DKN2O=0.0
!      DKNH3=0.0
!      DKHDO=0.0
      KHDO=0.0
      IF (.NOT. CFHDO) KHDO=0.0
!cc
!            Limit -DK so it can never totally cancel KFIX
             DK = DKCO2 + DKSO2 + DKHNO3 + DKN2O + DKNH3
             IF (-DK .GE. KFIX) THEN
                DK = -0.999*KFIX
             ENDIF
             
             KLAYER = KCON + KFIX + KW(ILAY) + KOZO + KHDO + DK
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
!  if( I .eq. 99) then
!    write(6,*) 'calt1(99): KCON,KFIX,KW,KOZO,KHDO,DK ',KCON,KFIX,KW,KOZO,KHDO,DK
!  endif
!             write(6,'(A,X,I4,3(X,E11.4))') 'calt1: ILAY,KHDO,KZ,KLAYER= ', ILAY,KHDO,KZ,KLAYER
          ENDDO
!         End loop on levels
!          write(6,'(A,X,I4,X,I4,X,E11.4)') 'calt1: I, IHDO= ', I,IHDO,KZ
!
!         Calc surface-to-space transmittance
          TAUZ(J)=QIKEXP(-KZ)
!
       ENDDO
!      End loops on channel number (frequency)
!
       RETURN
       END
