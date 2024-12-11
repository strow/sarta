!=======================================================================
 
! Code converted using TO_F90_LOOP by Alan Miller
! Date: 2023-04-04  Time: 16:44:55
 
!=======================================================================

!    University of Maryland Baltimore County [UMBC]

!    AIRS

!    CALT2 (for set2 = FOW) version with trace gases

!F77====================================================================


!ROUTINE NAME:
!    CALT2


!ABSTRACT:
!    Calculate the transmittance for set2 using the predictors and the
!    fast transmittance coefficients.


!CALL PROTOCOL:
!    CALT2 ( INDCHN, NLAY, NCHN2, CLIST2, COEF2, FIXMUL,
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
!    transmittances. Fixed, ozone, and water transmittances are each
!    checked individually to be sure they give 0 < trans < 1.

!    ===================================================================
!    Loops downward over all the layers for each of the NCHN2 channels
!    to compute the layer transmittances TAU.

!    The water continuum absorption coefficient is:
!       k_con = the sum i=1 to 5 of { COEF(i)*CONPRD(i) }

!    The layer effective fixed gas absorption coefficient is:
!       k_fixed = the sum i=1 to 8 of { COEF(5+i)*FPRED(i) }

!    The layer effective ozone absorption coefficient is:
!       k_ozone = the sum i=1 to 10 of { COEF(5+8+i)*OPRED(i) }

!    The layer effective water lines absorption coefficient is:
!       k_water = the sum i=1 to 11 of { COEF(5+8+10+i)*WPRED(i) }

!    where
!      "COEF" are the fast transmittance coefficients COEF2
!      "CONPRD" are the water continuum predictors CONPRD
!      "FPRED" are the fixed gases predictors FPRED2
!      "OPRED" are the ozone predictors OPRED2
!      "WPRED" are the water lines predictors WPRED2

!    The total layer effective optical depth TAU is:
!       TAU = [ k_con + k_fixed + k_ozone + k_water ]

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
!    28 Mar 2006 Scott Hannon   Change TAU from trans to optical depth
!    22 Dec 2006 Scott Hannon   Change TAUZ from trans to optical depth
!                               and from (1 x n) to (m x n) array;
!                               delete func QIKEXP & argument BLMULT.
!    14 Sep 2010 Scott Hannon   Add 5th CO2 coef
!    10 May 2018 C Hepplewhite  Add NH3
!    1  Feb 2019 C Hepplewhite  Add HDO

!END====================================================================

!      =================================================================

SUBROUTINE XCALT2 ( INDCHN, NLAY, NCHN2, CLIST2, COEF2,  &
    FIXMUL, CONPD2, FPRED2, OPRED2, WPRED2, DPRED, TRCPRD,  &
    INDCO2, COFCO2, CO2MLT, INDSO2, COFSO2, SO2MLT,  &
    INDHNO, COFHNO, HNOMLT, INDN2O, COFN2O, N2OMLT,  &
INDNH3, COFNH3, NH3MLT, INDHDO  COFHDO, HDOMLT, TAU, TAUZ )
  
!      =================================================================
  
!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------
  
  INTEGER, INTENT(IN)                      :: INDCHN(MXCHAN)
  INTEGER, INTENT(IN)                      :: NLAY
  INTEGER, INTENT(IN)                      :: NCHN2
  INTEGER, INTENT(IN)                      :: CLIST2(MXCHN2)
  REAL, INTENT(IN)                         :: COEF2(N2COEF,MAXLAY,MXCHN2)
  REAL, INTENT(IN)                         :: FIXMUL(MAXLAY)
  REAL, INTENT(IN)                         :: CONPD2( N2CON,MAXLAY)
  REAL, INTENT(IN)                         :: FPRED2( N2FIX,MAXLAY)
  REAL, INTENT(IN)                         :: OPRED2(  N2O3,MAXLAY)
  REAL, INTENT(IN)                         :: WPRED2( N2H2O,MAXLAY)
  REAL, INTENT(IN OUT)                     :: DPRED(   NHDO MAXLAY)
    REAL, INTENT(IN)                         :: TRCPRD(NTRACE,MAXLAY)
    INTEGER, INTENT(IN)                      :: INDCO2(MXCHAN)
    REAL, INTENT(IN)                         :: COFCO2(  NCO2,MAXLAY,MXCHNC)
    REAL, INTENT(IN)                         :: CO2MLT(MAXLAY)
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
    NO TYPE, INTENT(IN OUT)                  :: INDHDO  CO
      REAL, INTENT(IN OUT)                     :: HDOMLT(MAXLAY)
      REAL, INTENT(OUT)                        :: TAU(MAXLAY,MXCHAN)
      REAL, INTENT(OUT)                        :: TAUZ(MAXLAY,MXCHAN)
      IMPLICIT NONE
      
      
!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
      INCLUDE 'incFTC.f'
      
      
!-----------------------------------------------------------------------
!      EXTERNAL FUNCTIONS
!-----------------------------------------------------------------------
!      none
      
      
!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
!      Input
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      INTEGER :: INDHDO(MXCHAN)
      REAL :: COFHDO(  NHDO,MAXLAY,MXCHND)
      
      
!      Output
      
      
      
      
!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
      INTEGER :: I
      INTEGER :: ICO2
      INTEGER :: IHNO3
      INTEGER :: ILAY
      INTEGER :: IN2O
      INTEGER :: INH3
      INTEGER :: ISO2
      INTEGER :: IHDO
        INTEGER :: J
        REAL :: DK
        REAL :: DKCO2
        REAL :: DKHNO3
        REAL :: DKN2O
        REAL :: DKNH3
        REAL :: DKSO2
        REAL :: DKHDO
          REAL :: KHDO
            REAL :: KCON
            REAL :: KFIX
            REAL :: KLAYER
            REAL :: KOZO
            REAL :: KWAT
            REAL :: KZ
            LOGICAL :: LCO2
            LOGICAL :: LHNO3
            LOGICAL :: LN2O
            LOGICAL :: LNH3
            LOGICAL :: LSO2
            LOGICAL :: LHDO
              
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
              DO I=1,NCHN2
                
!         Index for TAU
                J=INDCHN( CLIST2(I) )
                
!         Determine whether or not to do variable CO2
                ICO2=INDCO2( CLIST2(I) )
                IF (ICO2 > 0) THEN
                  LCO2=.TRUE.
                ELSE
                  LCO2=.FALSE.
                END IF
                
!         Determine whether or not to do variable CO2
                ISO2=INDSO2( CLIST2(I) )
                IF (ISO2 > 0) THEN
                  LSO2=.TRUE.
                ELSE
                  LSO2=.FALSE.
                END IF
                
!         Determine whether or not to do variable HNO3
                IHNO3=INDHNO( CLIST2(I) )
                IF (IHNO3 > 0) THEN
                  LHNO3=.TRUE.
                ELSE
                  LHNO3=.FALSE.
                END IF
                
!         Determine whether or not to do variable N2O
                IN2O=INDN2O( CLIST2(I) )
                IF (IN2O > 0) THEN
                  LN2O=.TRUE.
                ELSE
                  LN2O=.FALSE.
                END IF
                
!         Determine whether or not to do variable NH3
                INH3=INDNH3( CLIST2(I) )
                IF (INH3 > 0) THEN
                  LNH3=.TRUE.
                ELSE
                  LNH3=.FALSE.
                END IF
                
!         Determine whether or not to do variable HDO calc
                IHDO=INDHDO( CLIST2(I) )
                IF (IHDO > 0) THEN
                  LHDO=.TRUE.
                ELSE
                  LHDO=.FALSE.
                END IF
                
!         Initialize the layer-to-space optical depth
                KZ=0.0E+0
                
!         ------------------------------
!         Loop on layers (top to ground)
!         ------------------------------
                DO ILAY=1,NLAY
                  
!            ---------------------------
!            Compute the water continuum
!            ---------------------------
                  KCON=( COEF2(1,ILAY,I)*CONPD2(1,ILAY) ) +  &
                      ( COEF2(2,ILAY,I)*CONPD2(2,ILAY) ) +  &
                      ( COEF2(3,ILAY,I)*CONPD2(3,ILAY) ) +  &
                      ( COEF2(4,ILAY,I)*CONPD2(4,ILAY) ) +  &
                      ( COEF2(5,ILAY,I)*CONPD2(5,ILAY) ) +  &
                      ( COEF2(6,ILAY,I)*CONPD2(6,ILAY) ) +  &
                      ( COEF2(7,ILAY,I)*CONPD2(7,ILAY) )
                  
                  IF (KCON < 0.0+0) THEN
                    KCON=0.0E+0
                  ELSE IF (KCON > 0.6E+0 .AND. ILAY == 1) THEN
!%%%%%                KCON=1.0E+1
                    KCON=1.0E-10
                  END IF
                  
                  
!            -----------------------------
!            Calc the fixed gases abs coef
!            -----------------------------
                  KFIX=( COEF2( 8,ILAY,I)*FPRED2(1,ILAY) ) +  &
                      ( COEF2( 9,ILAY,I)*FPRED2(2,ILAY) ) +  &
                      ( COEF2(10,ILAY,I)*FPRED2(3,ILAY) ) +  &
                      ( COEF2(11,ILAY,I)*FPRED2(4,ILAY) ) +  &
                      ( COEF2(12,ILAY,I)*FPRED2(5,ILAY) ) +  &
                      ( COEF2(13,ILAY,I)*FPRED2(6,ILAY) ) +  &
                      ( COEF2(14,ILAY,I)*FPRED2(7,ILAY) ) +  &
                      ( COEF2(15,ILAY,I)*FPRED2(8,ILAY) )
                  
                  KFIX=KFIX*FIXMUL(ILAY)
                  
                  IF (KFIX < 0.0E+0) THEN
                    KFIX=0.0E+0
                  ELSE IF (KFIX > 0.6E+0 .AND. ILAY == 1) THEN
!%%%%                KFIX=1.0E+1
                    KFIX=1.0E-10
                  END IF
                  
                  
!            --------------------------
!            Compute the ozone abs coef
!            --------------------------
                  KOZO=( COEF2(16,ILAY,I)*OPRED2( 1,ILAY) ) +  &
                      ( COEF2(17,ILAY,I)*OPRED2( 2,ILAY) ) +  &
                      ( COEF2(18,ILAY,I)*OPRED2( 3,ILAY) ) +  &
                      ( COEF2(19,ILAY,I)*OPRED2( 4,ILAY) ) +  &
                      ( COEF2(20,ILAY,I)*OPRED2( 5,ILAY) ) +  &
                      ( COEF2(21,ILAY,I)*OPRED2( 6,ILAY) ) +  &
                      ( COEF2(22,ILAY,I)*OPRED2( 7,ILAY) ) +  &
                      ( COEF2(23,ILAY,I)*OPRED2( 8,ILAY) ) +  &
                      ( COEF2(24,ILAY,I)*OPRED2( 9,ILAY) ) +  &
                      ( COEF2(25,ILAY,I)*OPRED2(10,ILAY) )
                  
                  IF (KOZO < 0.0E+0) THEN
                    KOZO=0.0E+0
                  ELSE IF (KOZO > 1.0E+0) THEN
!%%%%%%                KOZO=1.0E+1
                    KOZO=1.0E-10
                  END IF
                  
                  
!            --------------------------
!            Compute the water abs coef
!            --------------------------
                  KWAT=( COEF2(26,ILAY,I)*WPRED2( 1,ILAY) ) +  &
                      ( COEF2(27,ILAY,I)*WPRED2( 2,ILAY) ) +  &
                      ( COEF2(28,ILAY,I)*WPRED2( 3,ILAY) ) +  &
                      ( COEF2(29,ILAY,I)*WPRED2( 4,ILAY) ) +  &
                      ( COEF2(30,ILAY,I)*WPRED2( 5,ILAY) ) +  &
                      ( COEF2(31,ILAY,I)*WPRED2( 6,ILAY) ) +  &
                      ( COEF2(32,ILAY,I)*WPRED2( 7,ILAY) ) +  &
                      ( COEF2(33,ILAY,I)*WPRED2( 8,ILAY) ) +  &
                      ( COEF2(34,ILAY,I)*WPRED2( 9,ILAY) ) +  &
                      ( COEF2(35,ILAY,I)*WPRED2(10,ILAY) ) +  &
                      ( COEF2(36,ILAY,I)*WPRED2(11,ILAY) )
                  
                  IF (KWAT < 0.0E+0) THEN
                    KWAT=0.0E+0
                  ELSE IF( KWAT > 1.0E+0) THEN
!%%%%%%%%                KWAT=1.0E+1
                    KWAT=1.0E-10
                  END IF
                  
                  
!            ----------------------------------
!            Calc the total layer transmittance
!            ----------------------------------
                  
!cccc
! This block is usually commented out and is only uncommented for
! testing purposes.
                  
!           kcon=0.0E+0
!           kfix=0.0E+0
!           kozo=0.0E+0
!           kwat=0.0E+0
!cccc
                  
!            ----------------------------
!            Calc change in total optical
!            depth due to variable CO2
!            ----------------------------
                  IF (LCO2 .AND. CO2MLT(ILAY) /= 0) THEN
                    DKCO2=( COFCO2(1,ILAY,ICO2)*TRCPRD(1,ILAY) ) +  &
                        ( COFCO2(2,ILAY,ICO2)*TRCPRD(2,ILAY) ) +  &
                        ( COFCO2(3,ILAY,ICO2)*TRCPRD(3,ILAY) ) +  &
                        ( COFCO2(4,ILAY,ICO2)*TRCPRD(4,ILAY) ) +  &
                        ( COFCO2(5,ILAY,ICO2)*TRCPRD(5,ILAY) )
                    DKCO2=DKCO2*CO2MLT(ILAY)
                  ELSE
                    DKCO2=0.0
                  END IF
                  
                  
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
                  
!            ------------------------------------------
!            Calc total optical depth and transmittance
!            ------------------------------------------
!            Calc total layer optical depth
!cc
! this block for testing
!      DKCO2=0.0
!      DKSO2=0.0
!      DKHNO3=0.0
!      DKN2O=0.0
!       DKNH3=0.0
!       DKHDO=0.0
                  KHDO=0.0
!cc
!            Limit -DK so it can never totally totally cancel KFIX
                  DK = DKCO2 + DKSO2 + DKHNO3 + DKN2O + DKNH3
                  IF (-DK >= KFIX) THEN
                    DK = -0.999*KFIX
                  END IF
                  
!            Calc effective layer optical depth
                  KLAYER = KCON + KFIX + KOZO + KWAT + DK
                  TAU(ILAY,J)=KLAYER
                  
!            Calc layer-to-space optical depth
                  KZ=KZ + KLAYER
                  TAUZ(ILAY,J)=KZ
                  
                  ENDDO
!         End loop on levels
                    
                    ENDDO
!      End loops on channel number (frequency)
                      
                      RETURN
                    END SUBROUTINE XCALT2
