!=======================================================================
!    University of Maryland Baltimore County [UMBC]
!    AIRS
!    CALT3 (for set3 = FMW) version with trace gases (no CO2)
!F90====================================================================

!ROUTINE NAME:
!    CALT3

!ABSTRACT:
!    Calculate the transmittance for set3 using the prdictor and the
!    fast transmittance coefficients.

!CALL PROTOCOL:
!    CALT3 ( INDCHN, NLAY, NCHN3, CLIST3, COEF3,
!       FIXMUL, CONPD3, FPRED3, MPRED3, WPRED3, DPRED, TRCPRD,
!       INDSO2, COFSO2, SO2MLT, INDHNO, COFHNO, HNOMLT,
!       INDN2O, COFN2O, N2OMLT,
!       INDNH3, COFNH3, NH3MLT, INDHDO, COFHDO, HDOMLT,
!       INDH2O, H2OPRD, COFH2O, LOPMIN, LOPMAX,
!       LOPLOW, LOPUSE, WAOP, DAOP, WAANG, TAU, TAUZ)

!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INT arr   INDCHN  channel indices             none
!    INTEGER   NLAY    number of layers to bottom  none
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
!    REAL arr  TAU     effective layer opt depth   none
!    REAL arr  TAUZ    layer-to-space opt depth    none

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

!    The fast trans coefficents and predictors are multiplied
!    together and summed to calculate the effective layer
!    transmittances. Fixed, methane, and water transmittances are each
!    checked individually to be sure they give 0 < trans < 1.

!    ===================================================================
!    Loops downward over all the layers for each of the NCHN3 channels
!    to compute the layer transmittances TAU.

!    The water continuum absorption coefficient is:
!       k_con = the sum i=1 to 5 of { COEF(i)*CONPRD(i) }

!    The layer effective fixed gas absorption coefficient is:
!       k_fixed = the sum i=1 to 8 of { COEF(5+i)*FPRED(i) }

!    The layer effective methane absorption coefficient is:
!       k_methane = the sum i=1 to 9 of { COEF(5+8+i)*OPRED(i) }

!    The layer effective water lines absorption coefficient is:
!       k_water = the sum i=1 to 11 of { COEF(5+8+9+i)*WPRED(i) }

!    where
!      "COEF" are the fast transmittance coefficients COEF3
!      "CONPRD" are the water continuum predictors CONPRD
!      "FPRED" are the fixed gases predictors FPRED3
!      "MPRED" are the methane predictors OPRED3
!      "WPRED" are the water lines predictors WPRED3

!    The total layer effective optical depth is:
!       TAU = [ k_con + k_fixed + k_methane + k_water ]

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
!    28 Mar 2006 Scott Hannon   Change TAU from trans to optical depth
!    22 Dec 2006 Scott Hannon   Change TAUZ from trans to optical depth
!                               and from (1 x n) to (m x n) array;
!                               delete func QIKEXP & argument BLMULT.
!    10 May 2018 C Hepplewhite  Add NH3
!    1  Feb 2019 C Hepplewhite  Add HDO

!END====================================================================

!      =================================================================

SUBROUTINE XCALT3 ( INDCHN, NLAY, NCHN3, CLIST3, COEF3,  &
    FIXMUL, CONPD3, FPRED3, MPRED3, WPRED3, DPRED, TRCPRD,  &
    INDSO2, COFSO2, SO2MLT, INDHNO, COFHNO, HNOMLT,  &
    INDN2O, COFN2O, N2OMLT, INDNH3, COFNH3, NH3MLT,  &
    INDHDO, COFHDO, HDOMLT, INDH2O, H2OPRD, COFH2O,  &
    LOPMIN, LOPMAX, LOPLOW, LOPUSE, WAOP, DAOP, WAANG, TAU, TAUZ)
!    =================================================================
  
!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
USE incFTC
      
!-----------------------------------------------------------------------
IMPLICIT NONE
!-----------------------------------------------------------------------
  
!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
      
  INTEGER, INTENT(IN)                      :: INDCHN(MXCHAN)
  INTEGER, INTENT(IN)                      :: NLAY
  INTEGER, INTENT(IN)                      :: NCHN3
  INTEGER, INTENT(IN)                      :: CLIST3(MXCHN3)
  REAL, INTENT(IN)                         :: COEF3(N3COEF,MAXLAY,MXCHN3)
  REAL, INTENT(IN)                         :: FIXMUL(MAXLAY)
  REAL, INTENT(IN)                         :: CONPD3( N3CON,MAXLAY)
  REAL, INTENT(IN)                         :: FPRED3( N3FIX,MAXLAY)
  REAL, INTENT(IN)                         :: MPRED3( N3CH4,MAXLAY)
  REAL, INTENT(IN)                         :: WPRED3( N3H2O,MAXLAY)
  REAL, INTENT(IN)                         :: DPRED(  NHDO, MAXLAY)
  REAL, INTENT(IN)                         :: TRCPRD(NTRACE,MAXLAY)
  INTEGER, INTENT(IN)                      :: INDSO2(MXCHAN)
  REAL, INTENT(IN)                         :: COFSO2(  NSO2,MAXLAY,MXCHNS)
  REAL, INTENT(IN)                         :: SO2MLT(MAXLAY)
  INTEGER, INTENT(IN)                      :: INDHNO(MXCHAN)
  REAL, INTENT(IN)                         :: COFHNO( NHNO3,MAXLAY,MXCHNH)
  REAL, INTENT(IN)                         :: HNOMLT(MAXLAY)
  INTEGER, INTENT(IN)                      :: INDN2O(MXCHAN)
  REAL, INTENT(IN)                         :: COFN2O(  NN2O,MAXLAY,MXCHNN)
  REAL, INTENT(IN)                         :: N2OMLT(MAXLAY)
  INTEGER, INTENT(IN)                      :: INDNH3(MXCHAN)
  REAL, INTENT(IN)                         :: COFNH3(  NNH3,MAXLAY,MXCHNA)
  REAL, INTENT(IN)                         :: NH3MLT(MAXLAY)
  INTEGER, INTENT(IN)                      :: INDHDO(MXCHAN)
  REAL, INTENT(IN)                         :: HDOMLT(MAXLAY)
  REAL                                     :: COFHDO(  NHDO,MAXLAY,MXCHND)
  INTEGER, INTENT(IN)                      :: INDH2O(MXCHAN)
  REAL, INTENT(IN)                         :: H2OPRD(  NH2O,MXOWLY)
  REAL, INTENT(IN)                         :: COFH2O(  NH2O,MXOWLY,MXCHNW)
  INTEGER, INTENT(IN)                      :: LOPMIN
  INTEGER, INTENT(IN)                      :: LOPMAX
  INTEGER, INTENT(IN)                      :: LOPLOW(MAXLAY)
  LOGICAL, INTENT(IN)                      :: LOPUSE(MXOWLY)
  REAL, INTENT(IN)                         :: WAOP(MXOWLY)
  REAL, INTENT(IN)                         :: DAOP(MAXLAY)
  REAL, INTENT(IN)                         :: WAANG(MAXLAY)
!
  REAL, INTENT(OUT)                        :: TAU(MAXLAY,MXCHAN)
  REAL, INTENT(OUT)                        :: TAUZ(MAXLAY,MXCHAN)
      
!-----------------------------------------------------------------------
!      EXTERNAL FUNCTIONS
!-----------------------------------------------------------------------
!      none
      
!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
      INTEGER :: I
      INTEGER :: IHNO3
      INTEGER :: IN2O
      INTEGER :: ILAY
      INTEGER :: ISO2
      INTEGER :: INH3
      INTEGER :: IHDO
      INTEGER :: J
      REAL :: DK
      REAL :: DKHNO3
      REAL :: DKN2O
      REAL :: DKSO2
      REAL :: DKNH3
      REAL :: DKHDO
      REAL :: KHDO
      REAL :: KCON
      REAL :: KFIX
      REAL :: KMET
      REAL :: KZ
      REAL :: KZFMW
      REAL :: KLAYER
      LOGICAL :: LH2O
      LOGICAL :: LHNO3
      LOGICAL :: LN2O
      LOGICAL :: LSO2
      LOGICAL :: LNH3
      LOGICAL :: LHDO
              
!      for CALOKW
     INTEGER :: IH2O
     REAL :: KW(MAXLAY)
              
!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none
              
!***********************************************************************
!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!***********************************************************************
              
!      ---------------------------
!      Loop on channel (frequency)
!      ---------------------------
       DO I=1,NCHN3
                
!         Index for TAU
          J=INDCHN( CLIST3(I) )
                
!         Determine whether or not to do variable SO2
             ISO2=INDSO2( CLIST3(I) )
             IF (ISO2 > 0) THEN
                LSO2=.TRUE.
             ELSE
                LSO2=.FALSE.
             END IF
                
!         Determine whether or not to do variable HNO3
            IHNO3=INDHNO( CLIST3(I) )
            IF (IHNO3 > 0) THEN
               LHNO3=.TRUE.
            ELSE
               LHNO3=.FALSE.
            END IF
                
!         Determine whether or not to do variable N2O
            IN2O=INDN2O( CLIST3(I) )
            IF (IN2O > 0) THEN
               LN2O=.TRUE.
            ELSE
               LN2O=.FALSE.
            END IF
                
!         Determine whether or not to do variable NH3
            INH3=INDNH3( CLIST3(I) )
            IF (INH3 > 0) THEN
               LNH3=.TRUE.
            ELSE
               LNH3=.FALSE.
            END IF
                
!         Determine whether or not to do variable HDO calc
            IHDO=INDHDO( CLIST3(I) )
              IF (IHDO > 0) THEN
                LHDO=.TRUE.
              ELSE
                LHDO=.FALSE.
              END IF
                
!         -------------------------
!         Do OPTRAN water if needed
!         -------------------------
             IH2O=INDH2O( CLIST3(I) )
             IF (IH2O > 0) THEN
                LH2O=.FALSE.
!            Calc OPTRAN water
                  
                CALL CALOKW( NLAY, IH2O, LOPMIN, LOPMAX, LOPLOW, LOPUSE,  &
                      H2OPRD, COFH2O, WAOP, DAOP, WAANG, KW )
             ELSE
                LH2O=.TRUE.
             END IF
                
!         Initialize the layer-to-space optical depth
             KZ=0.0E+0
             KZFMW=0.0E+0
                
!         ------------------------------
!         Loop on layers (top to ground)
!         ------------------------------
             DO ILAY=1,NLAY
                  
!            ---------------------------
!            Compute the water continuum
!            ---------------------------
                KCON=( COEF3(1,ILAY,I)*CONPD3(1,ILAY) ) +  &
                      ( COEF3(2,ILAY,I)*CONPD3(2,ILAY) ) +  &
                      ( COEF3(3,ILAY,I)*CONPD3(3,ILAY) ) +  &
                      ( COEF3(4,ILAY,I)*CONPD3(4,ILAY) ) +  &
                      ( COEF3(5,ILAY,I)*CONPD3(5,ILAY) ) +  &
                      ( COEF3(6,ILAY,I)*CONPD3(6,ILAY) ) +  &
                      ( COEF3(7,ILAY,I)*CONPD3(7,ILAY) )
                  
                IF (KCON < 0.0E+0) THEN
                   KCON=0.0E+0
                ELSE IF (KCON > 1.0E+1) THEN
                    KCON=1.0E+1
                END IF
                  
!            -----------------------------
!            Calc the fixed gases abs coef
!            -----------------------------
               KFIX=( COEF3( 8,ILAY,I)*FPRED3(1,ILAY) ) +  &
                    ( COEF3( 9,ILAY,I)*FPRED3(2,ILAY) ) +  &
                    ( COEF3(10,ILAY,I)*FPRED3(3,ILAY) ) +  &
                    ( COEF3(11,ILAY,I)*FPRED3(4,ILAY) ) +  &
                    ( COEF3(12,ILAY,I)*FPRED3(5,ILAY) ) +  &
                    ( COEF3(13,ILAY,I)*FPRED3(6,ILAY) ) +  &
                    ( COEF3(14,ILAY,I)*FPRED3(7,ILAY) ) +  &
                    ( COEF3(15,ILAY,I)*FPRED3(8,ILAY) )
                  
               KFIX=KFIX*FIXMUL(ILAY)
                  
               IF (KFIX < 0.0E+0) THEN
                  KFIX=0.0E+0
               ELSE IF (KFIX > 1.0E+1) THEN
                  KFIX=1.0E+1
               END IF
                  
!            ----------------------------
!            Compute the methane abs coef
!            ----------------------------
               KMET=( COEF3(16,ILAY,I)*MPRED3(1,ILAY) ) +  &
                      ( COEF3(17,ILAY,I)*MPRED3(2,ILAY) ) +  &
                      ( COEF3(18,ILAY,I)*MPRED3(3,ILAY) ) +  &
                      ( COEF3(19,ILAY,I)*MPRED3(4,ILAY) ) +  &
                      ( COEF3(20,ILAY,I)*MPRED3(5,ILAY) ) +  &
                      ( COEF3(21,ILAY,I)*MPRED3(6,ILAY) ) +  &
                      ( COEF3(22,ILAY,I)*MPRED3(7,ILAY) ) +  &
                      ( COEF3(23,ILAY,I)*MPRED3(8,ILAY) ) +  &
                      ( COEF3(24,ILAY,I)*MPRED3(9,ILAY) )
                  
               IF (KMET < 0.0E+0) THEN
                  KMET=0.0E+0
               ELSE IF (KMET > 1.0E+1) THEN
                  KMET=1.0E+1
               END IF
                  
!            --------------------------
!            Compute the water abs coef
!            --------------------------
               IF (LH2O) THEN
!               Not an OPTRAN water channel
                    KW(ILAY)= ( COEF3(25,ILAY,I)*WPRED3( 1,ILAY) ) +  &
                        ( COEF3(26,ILAY,I)*WPRED3( 2,ILAY) ) +  &
                        ( COEF3(27,ILAY,I)*WPRED3( 3,ILAY) ) +  &
                        ( COEF3(28,ILAY,I)*WPRED3( 4,ILAY) ) +  &
                        ( COEF3(29,ILAY,I)*WPRED3( 5,ILAY) ) +  &
                        ( COEF3(30,ILAY,I)*WPRED3( 6,ILAY) ) +  &
                        ( COEF3(31,ILAY,I)*WPRED3( 7,ILAY) ) +  &
                        ( COEF3(32,ILAY,I)*WPRED3( 8,ILAY) ) +  &
                        ( COEF3(33,ILAY,I)*WPRED3( 9,ILAY) ) +  &
                        ( COEF3(34,ILAY,I)*WPRED3(10,ILAY) ) +  &
                        ( COEF3(35,ILAY,I)*WPRED3(11,ILAY) )
                   
                    IF (KW(ILAY) < 0.0E+0) KW(ILAY)=0.0E+0
               END IF
                  
!            Update KZFMW
               KZFMW=KZFMW + KFIX + KMET + KW(ILAY)
                  
!            --------------------------
!            Compute the HDO abs coef
!            --------------------------
              IF (LHDO) THEN
                  KHDO=( COFHDO(1,ILAY,IHDO)*DPRED( 1,ILAY) ) +  &
                        ( COFHDO(2,ILAY,IHDO)*DPRED( 2,ILAY) ) +  &
                        ( COFHDO(3,ILAY,IHDO)*DPRED( 3,ILAY) ) +  &
                        ( COFHDO(4,ILAY,IHDO)*DPRED( 4,ILAY) ) +  &
                        ( COFHDO(5,ILAY,IHDO)*DPRED( 5,ILAY) ) +  &
                        ( COFHDO(6,ILAY,IHDO)*DPRED( 6,ILAY) ) +  &
                        ( COFHDO(7,ILAY,IHDO)*DPRED( 7,ILAY) ) +  &
                        ( COFHDO(8,ILAY,IHDO)*DPRED( 8,ILAY) )
!     $               ( COFHDO(9,ILAY,IHDO)*DPRED( 9,ILAY) ) +
!     $               ( COFHDO(10,ILAY,IHDO)*DPRED(10,ILAY) ) +
!     $               ( COFHDO(11,ILAY,IHDO)*DPRED(11,ILAY) )
                    
!                IF (KHDO .LT. 0.0E+0) KHDO=0.0E+0
                    KHDO=KHDO*HDOMLT(ILAY)
              ELSE
                  KHDO=0.0
              END IF
                  
!            ----------------------------------
!            Calc the total layer transmittance
!            ----------------------------------
                  
!cccc
! This block is usually commented out and is only uncommented for
! testing purposes.
                  
!           kcon=0.0
!           kfix=0.0
!           kmet=0.0
!           kw(ilay)=0.0
!cccc
!            ----------------------------
!            Calc change in total optical
!            depth due to variable SO2
!            ----------------------------
              IF (LSO2 .AND. SO2MLT(ILAY) /= 0) THEN
                  DKSO2=( COFSO2(1,ILAY,ISO2)*TRCPRD(1,ILAY) ) +  &
                        ( COFSO2(2,ILAY,ISO2)*TRCPRD(2,ILAY) ) +  &
                        ( COFSO2(3,ILAY,ISO2)*TRCPRD(3,ILAY) ) +  &
                        ( COFSO2(4,ILAY,ISO2)*TRCPRD(4,ILAY) )
                  DKSO2=DKSO2*SO2MLT(ILAY)
              ELSE
                  DKSO2=0.0
              END IF
                  
!            ----------------------------
!            Calc change in total optical
!            depth due to variable HNO3
!            ----------------------------
              IF (LHNO3 .AND. HNOMLT(ILAY) /= 0) THEN
                  DKHNO3=( COFHNO(1,ILAY,IHNO3)*TRCPRD(1,ILAY) ) +  &
                        ( COFHNO(2,ILAY,IHNO3)*TRCPRD(2,ILAY) ) +  &
                        ( COFHNO(3,ILAY,IHNO3)*TRCPRD(3,ILAY) ) +  &
                        ( COFHNO(4,ILAY,IHNO3)*TRCPRD(4,ILAY) )
                  DKHNO3=DKHNO3*HNOMLT(ILAY)
              ELSE
                  DKHNO3=0.0
              END IF
                  
!            ----------------------------
!            Calc change in total optical
!            depth due to variable N2O
!            ----------------------------
              IF (LN2O .AND. N2OMLT(ILAY) /= 0) THEN
                    DKN2O=( COFN2O(1,ILAY,IN2O)*TRCPRD(1,ILAY) ) +  &
                        ( COFN2O(2,ILAY,IN2O)*TRCPRD(2,ILAY) ) +  &
                        ( COFN2O(3,ILAY,IN2O)*TRCPRD(3,ILAY) ) +  &
                        ( COFN2O(4,ILAY,IN2O)*TRCPRD(4,ILAY) ) +  &
                        ( COFN2O(5,ILAY,IN2O)*TRCPRD(5,ILAY) ) +  &
                        ( COFN2O(6,ILAY,IN2O)*TRCPRD(6,ILAY) ) +  &
                        ( COFN2O(7,ILAY,IN2O)*TRCPRD(7,ILAY) )
                    DKN2O=DKN2O*N2OMLT(ILAY)
              ELSE
                   DKN2O=0.0
              END IF
                  
!            ----------------------------
!            Calc change in total optical
!            depth due to variable NH3
!            ----------------------------
              IF (LNH3 .AND. NH3MLT(ILAY) /= 0) THEN
                    DKNH3=( COFNH3(1,ILAY,INH3)*TRCPRD(1,ILAY) ) +  &
                        ( COFNH3(2,ILAY,INH3)*TRCPRD(2,ILAY) ) +  &
                        ( COFNH3(3,ILAY,INH3)*TRCPRD(3,ILAY) ) +  &
                        ( COFNH3(4,ILAY,INH3)*TRCPRD(4,ILAY) )
                    DKNH3=DKNH3*NH3MLT(ILAY)
              ELSE
                   DKNH3=0.0
              END IF
                  
!cc
! this block for testing
!      DKSO2=0.0
!      DKHNO3=0.0
!      DKN2O=0.0
!       DKNH3=0.0
!       DKHDO=0.0
                  KHDO=0.0
!cc
!            Limit -DK so it can never totally totally cancel KFIX
              DK = DKSO2 + DKHNO3 + DKN2O + DKNH3
              IF (-DK >= KFIX) THEN
                  DK = -0.999*KFIX
              END IF
                  
!            Calc total layer optical depth
              KLAYER = KCON + KFIX + KMET + KW(ILAY) + DK
              TAU(ILAY,J)=KLAYER
                  
!            Calc layer-to-space optical depth
              KZ=KZ + KLAYER
              TAUZ(ILAY,J)=KZ
                  
          ENDDO
!         End loop on levels
                    
       ENDDO
!      End loops on channel number (frequency)
                      
       RETURN
       END SUBROUTINE XCALT3
