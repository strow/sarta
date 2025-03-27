!=======================================================================
!
!    University of Maryland Baltimore County [UMBC]
!
!    AIRS
!
!    CALT3 (for set3 = FMW) version with NH3, HDO
!
!F90====================================================================


!ROUTINE NAME:
!    CALT3


!ABSTRACT:
!    Calculate the transmittance for set3 using the prdictor and the
!    fast transmittance coefficients.


!CALL PROTOCOL:
!    CALT3 ( INDCHN, NLAY, BLMULT, NCHN3, CLIST3, COEF3,
!       FIXMUL, CONPD3, FPRED3, MPRED3, WPRED3, DPRED. TRCPRD,
!       INDSO2, COFSO2, SO2MLT, INDHNO, COFHNO, HNOMLT,
!       INDN2O, COFN2O, N2OMLT, INDNH3, COFNH3, NH3MLT,
!       INDHDO, COFHDO, HDOMLT,
!       INDH2O, H2OPRD, COFH2O, LOPMIN, LOPMAX,
!       LOPLOW, LOPUSE, WAOP, DAOP, WAANG, TAU, TAUZ)


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INT arr   INDCHN  channel indices             none
!    INTEGER   NLAY    number of layers to bottom  none
!    REAL      BLMULT  bottom layer opt depth mult none
!    INTEGER   NCHN3   set3 number of channels     none
!    INT arr   CLIST3  set3 channel list           none
!    REAL arr  COEF3   set3 fast trans coefs       various
!    REAL arr  FIXMUL  fixed amount mult (~1.0)    none
!    REAL arr  CONPD3  set3 H2O continuum preds    various
!    REAL arr  FPRED3  set3 fixed gases preds      various
!    REAL arr  MPRED3  set3 methane predictors     various
!    REAL arr  WPRED3  set3 water predictors       various
!    REAL arr  DPRED   HDO predictors              various
!    REAL arr  TRCPRD  trace gas pert predictors   various
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
!    The fast trans coefficents and predictors are multiplied
!    together and summed to calculate the effective layer
!    transmittances. Fixed, methane, and water transmittances are each
!    checked individually to be sure they give 0 < trans < 1.
!
!    ===================================================================
!    Loops downward over all the layers for each of the NCHN3 channels
!    to compute the layer transmittances TAU.
!
!    The water continuum absorption coefficient is:
!       k_con = the sum i=1 to 5 of { COEF(i)*CONPRD(i) }
!
!    The layer effective fixed gas absorption coefficient is:
!       k_fixed = the sum i=1 to 8 of { COEF(5+i)*FPRED(i) }
!
!    The layer effective methane absorption coefficient is:
!       k_methane = the sum i=1 to 9 of { COEF(5+8+i)*OPRED(i) }
!
!    The layer effective water lines absorption coefficient is:
!       k_water = the sum i=1 to 11 of { COEF(5+8+9+i)*WPRED(i) }
!
!    where
!      "COEF" are the fast transmittance coefficients COEF3
!      "CONPRD" are the water continuum predictors CONPRD
!      "FPRED" are the fixed gases predictors FPRED3
!      "MPRED" are the methane predictors OPRED3
!      "WPRED" are the water lines predictors WPRED3
!
!    The total layer effective transmittance is:
!       TAU = exp( -[ k_con + k_fixed + k_methane + k_water])
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
!     3 Feb 1997 Scott Hannon   Re-wrote (from CALTAU) for FMW
!     3 Sep 1997 Scott Hannon   Added TAUZ and BLMULT
!     5 Mar 1998 Scott Hannon   Added OPTRAN water and deleted water
!                               preds 12 & 13
!     4 May 1998 Scott Hannon   Fix error: INDH2O(MXCHAN) not (MXCHNW)
!    26 Aug 1998 Scott Hannon   Fix mistake: loop on NLAY not MAXLAY;
!                               Add NLAY to call to CALOKW
!    11 Aug 2000 Scott Hannon   Change from 4 to 5 term H2O continuum
!    12 Sep 2002 Scott Hannon   Add predictors 6 & 7 to H2O con
!    25 Apr 2003 Scott Hannon   Add HNO3 based on SO2 code
!    28 Jun 2005 Scott Hannon   "trace" version for SO2,HNO3,N2O
!    10 May 2018 C Hepplewhite  Add NH3
!    1  Feb 2019 C Hepplewhite  Add HDO

!END====================================================================

!      =================================================================
       SUBROUTINE CALT3 ( INDCHN, NLAY, BLMULT, NCHN3, CLIST3, COEF3, &
          FIXMUL, CONPD3, FPRED3, MPRED3, WPRED3, DPRED, TRCPRD, &
          INDSO2, COFSO2, SO2MLT, INDHNO, COFHNO, HNOMLT, &
          INDN2O, COFN2O, N2OMLT, INDNH3, COFNH3, NH3MLT, &
          INDHDO, COFHDO, HDOMLT, INDH2O, H2OPRD, COFH2O, &
          LOPMIN, LOPMAX, LOPLOW, LOPUSE, WAOP, DAOP, WAANG, TAU, TAUZ)
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
       INTEGER  NCHN3
       INTEGER CLIST3(MXCHN3)
       REAL  COEF3(N3COEF,MAXLAY,MXCHN3)
       REAL FIXMUL(MAXLAY)
       REAL CONPD3( N3CON,MAXLAY)
       REAL FPRED3( N3FIX,MAXLAY)
       REAL MPRED3( N3CH4,MAXLAY)
       REAL WPRED3( N3H2O,MAXLAY) 
       REAL DPRED(   NHDO,MAXLAY)
       REAL TRCPRD(NTRACE,MAXLAY)
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
       REAL    TAU(MAXLAY,MXCHAN)
       REAL   TAUZ(MXCHAN)


!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
integer :: I, IHNO3, IN2O, INH3, ILAY, ISO2, IHDO, J
real(4) ::  DK, DKHNO3, DKN2O, DKNH3, DKSO2, DKHDO, KHDO
real(4) ::  KCON, KFIX, KMET, KZ, KZFMW, KLAYER
logical ::  LH2O,  LHNO3, LN2O, LNH3, LSO2, LHDO
!
!      for function QIKEXP
       REAL QIKEXP
!
!      for CALOKW
INTEGER ::  IH2O
real(4), dimension(MAXLAY) :: KW

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
       DO I=1,NCHN3
!
!         Index for TAU
          J=INDCHN( CLIST3(I) )
!  if(DEBUG) print*,'calt3: J ', J
!
!
!         Determine whether or not to do variable SO2
          ISO2=INDSO2( CLIST3(I) )
          IF (ISO2 .GT. 0) THEN
             LSO2=.TRUE.
          ELSE
             LSO2=.FALSE.
          ENDIF
!
!         Determine whether or not to do variable HNO3
          IHNO3=INDHNO( CLIST3(I) )
          IF (IHNO3 .GT. 0) THEN
             LHNO3=.TRUE.
          ELSE
             LHNO3=.FALSE.
          ENDIF
!
!         Determine whether or not to do variable N2O
          IN2O=INDN2O( CLIST3(I) )
          IF (IN2O .GT. 0) THEN
             LN2O=.TRUE.
          ELSE
             LN2O=.FALSE.
          ENDIF
!
!         Determine whether or not to do variable NH3
          INH3=INDNH3( CLIST3(I) )
          IF (INH3 .GT. 0) THEN
             LNH3=.TRUE.
          ELSE
             LNH3=.FALSE.
          ENDIF
!
!         Determine whether or not to do variable HDO calc
          IHDO=INDHDO( CLIST3(I) )
          IF (IHDO .GT. 0) THEN
             LHDO=.TRUE.
          ELSE
             LHDO=.FALSE.
          ENDIF
!
!         -------------------------
!         Do OPTRAN water if needed
!         -------------------------
          IH2O=INDH2O( CLIST3(I) )
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
!   if(DEBUG) print*,'calt3: completed calokw'
!
!         Initialize the layer-to-space optical depth
          KZ=0.0E+0
          KZFMW=0.0E+0
!
!         ------------------------------
!         Loop on layers (top to ground)
!         ------------------------------
          DO ILAY=1,NLAY
!
!            ---------------------------
!            Compute the water continuum
!            ---------------------------
             KCON=( COEF3(1,ILAY,I)*CONPD3(1,ILAY) ) + &
                  ( COEF3(2,ILAY,I)*CONPD3(2,ILAY) ) + &
                  ( COEF3(3,ILAY,I)*CONPD3(3,ILAY) ) + &
                  ( COEF3(4,ILAY,I)*CONPD3(4,ILAY) ) + &
                  ( COEF3(5,ILAY,I)*CONPD3(5,ILAY) ) + &
                  ( COEF3(6,ILAY,I)*CONPD3(6,ILAY) ) + &
                  ( COEF3(7,ILAY,I)*CONPD3(7,ILAY) )
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
             KFIX=( COEF3( 8,ILAY,I)*FPRED3(1,ILAY) ) + &
                  ( COEF3( 9,ILAY,I)*FPRED3(2,ILAY) ) + &
                  ( COEF3(10,ILAY,I)*FPRED3(3,ILAY) ) + &
                  ( COEF3(11,ILAY,I)*FPRED3(4,ILAY) ) + &
                  ( COEF3(12,ILAY,I)*FPRED3(5,ILAY) ) + &
                  ( COEF3(13,ILAY,I)*FPRED3(6,ILAY) ) + &
                  ( COEF3(14,ILAY,I)*FPRED3(7,ILAY) ) + &
                  ( COEF3(15,ILAY,I)*FPRED3(8,ILAY) )
!
             KFIX=KFIX*FIXMUL(ILAY)
!
             IF (KFIX .LT. 0.0E+0) THEN
                KFIX=0.0E+0
             ELSEIF (KFIX .GT. 1.0E+1) THEN
                KFIX=1.0E+1
             ENDIF
!
!            ----------------------------
!            Compute the methane abs coef
!            ----------------------------
             KMET=( COEF3(16,ILAY,I)*MPRED3(1,ILAY) ) + &
                  ( COEF3(17,ILAY,I)*MPRED3(2,ILAY) ) + &
                  ( COEF3(18,ILAY,I)*MPRED3(3,ILAY) ) + &
                  ( COEF3(19,ILAY,I)*MPRED3(4,ILAY) ) + &
                  ( COEF3(20,ILAY,I)*MPRED3(5,ILAY) ) + &
                  ( COEF3(21,ILAY,I)*MPRED3(6,ILAY) ) + &
                  ( COEF3(22,ILAY,I)*MPRED3(7,ILAY) ) + &
                  ( COEF3(23,ILAY,I)*MPRED3(8,ILAY) ) + &
                  ( COEF3(24,ILAY,I)*MPRED3(9,ILAY) )
!
             IF (KMET .LT. 0.0E+0) THEN
                KMET=0.0E+0
             ELSEIF (KMET .GT. 1.0E+1) THEN
                KMET=1.0E+1
             ENDIF
!
!            --------------------------
!            Compute the water abs coef
!            --------------------------
             IF (LH2O) THEN
!               Not an OPTRAN water channel
                KW(ILAY)= &
                     ( COEF3(25,ILAY,I)*WPRED3( 1,ILAY) ) + &
                     ( COEF3(26,ILAY,I)*WPRED3( 2,ILAY) ) + &
                     ( COEF3(27,ILAY,I)*WPRED3( 3,ILAY) ) + &
                     ( COEF3(28,ILAY,I)*WPRED3( 4,ILAY) ) + &
                     ( COEF3(29,ILAY,I)*WPRED3( 5,ILAY) ) + &
                     ( COEF3(30,ILAY,I)*WPRED3( 6,ILAY) ) + &
                     ( COEF3(31,ILAY,I)*WPRED3( 7,ILAY) ) + &
                     ( COEF3(32,ILAY,I)*WPRED3( 8,ILAY) ) + &
                     ( COEF3(33,ILAY,I)*WPRED3( 9,ILAY) ) + &
                     ( COEF3(34,ILAY,I)*WPRED3(10,ILAY) ) + &
                     ( COEF3(35,ILAY,I)*WPRED3(11,ILAY) )
!
                IF (KW(ILAY) .LT. 0.0E+0) KW(ILAY)=0.0E+0
             ENDIF
!
!            Update KZFMW
             KZFMW=KZFMW + KFIX + KMET + KW(ILAY)
!
!            --------------------------
!            Compute the HDO abs coef
!            --------------------------
             IF (LHDO) THEN
                KHDO = 0.0
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

!                IF (KHDO .LT. 0.0E+0) KHDO=ABS(KHDO)    ! 0.0E+0
                IF (ABS(KHDO) .GT. 0.999) KHDO = 0.999
                KHDO=KHDO*HDOMLT(ILAY)
             ELSE
                KHDO=0.0
             ENDIF
!
  if(DEBUG .AND. J .EQ. 842) then
    print*,'calt3: J,KHDO,HDOMLT ', J,KHDO, HDOMLT(ILAY)
  endif
!            ----------------------------------
!            Calc the total layer transmittance
!            ----------------------------------
!
!cccc
! This block is usually commented out and is only uncommented for
! testing purposes.
!
!  KHDO = 0.0
! kcon=0.0
! kfix=0.0
! kmet=0.0
! kw(ilay)=0.0
!cccc
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

!            Calc the total layer effective optical depth
!cc
! this block for testing
!       DKSO2=0.0
!       DKHNO3=0.0
!       DKN2O=0.0
!       DKNH3=0.0
!       DKHDO=0.0
!       KHDO=0.0
      IF (.NOT. CFHDO) KHDO=0.0
!cc
!            Limit -DK so it can never totally totally cancel KFIX
             DK = DKSO2 + DKHNO3 + DKN2O + DKNH3
             IF (-DK .GE. KFIX) THEN
                DK = -0.999*KFIX
             ENDIF
!  if(DEBUG) print*,'calt3: KCON,KFIX,KMET,KW,KHDO,DK ', &
!         KCON,KFIX,KMET,KW(ILAY),KHDO,DK
!  if(DEBUG) print*,'   calt3: DKSO2,DKHNO3,DKN2O,DKNH3 ', &
!         DKSO2,DKHNO3,DKN2O,DKNH3

             KLAYER = KCON + KFIX + KMET + KW(ILAY) + KHDO + DK
!
!            Adjust the optical depth of the bottom layer
             IF (ILAY .EQ. NLAY) KLAYER=BLMULT*KLAYER
!
             KZ=KZ + KLAYER
!  if(DEBUG) print*,'calt3: qikexp(-klayer) ', QIKEXP(-KLAYER)
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
