!=======================================================================
 
! Code converted using TO_F90_LOOP by Alan Miller
! Date: 2023-04-04  Time: 16:44:55
 
!=======================================================================

!    University of Maryland Baltimore County [UMBC]

!    AIRS

!    CALT7 (set7=FWO sun mfbw) version for trace gases (CO2, N2O, HDO)
!    (no SO2 or HNO3 or NH3)
!    with s/w CH4

!F77====================================================================


!ROUTINE NAME:
!    CALT7


!ABSTRACT:
!    Calculate the transmittance for set7 using the predictors and the
!    fast transmittance coefficients.


!CALL PROTOCOL:
!    CALT7( LTAU, INDCHN, NLAY, BLMULT, NCHN7, CLIST7, COEF7,
!       FIXMUL, CONPD7, FPRED7, WPRED7, OPRED7, TRCPRD, INDCO2, COFCO2,
!       CO2MLT, INDN2O, COFN2O, N2OMLT, INDCH4, COFCH4, CH4MLT,
!       XZ, TAU, TAUZ )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    LOGICAL   LTAU    calc all layer trans?       none
!    INT arr   INDCHN  channel indices             none
!    INTEGER   NLAY    number of layers to bottom  none
!    REAL      BLMULT  bottom layer opt depth mult none
!    INTEGER   NCHN7   set7 number of channels     none
!    INT arr   CLIST7  set7 channel list           none
!    REAL arr  COEF7   set7 fast trans coefs       various
!    REAL arr  FIXMUL  fixed amount mult (~1.0)    none
!    REAL arr  CONPD7  set7 H2O continuum preds    various
!    REAL arr  FPRED7  set7 fixed gases preds      various
!    REAL arr  WPRED7  set7 water predictors       various
!    REAL arr  OPRED7  set7 ozone predictors       various
!    REAL arr  TRCPRD  trace gas pert predictors   various
!    INT arr   INDCO2  CO2 pert chan indices       none
!    REAL arr  COFCO2  CO2 pert coefs              various
!    REAL arr  CO2MLT  CO2 pert multiplier         none
!    INT arr   INDN2O  N2O pert chan indices       none
!    REAL arr  COFN2O  N2O pert coefs              various
!    REAL arr  N2OMLT  N2O pert multiplier         none
!    INT arr   INDHDO  HDO pert chan indices       none
!    REAL arr  COFHDO  HDO pert coefs              various
!    REAL arr  HDOMLT  HDO pert multiplier         none
!    INT arr   INDCH4  CH4 pert chan indices       none
!    REAL arr  COFCH4  CH4 pert coefs              various
!    REAL arr  CH4MLT  CH4 pert multiplier         none
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

!    The fast trans coefficents and predictors are multiplied
!    together and summed to calculate the effective layer
!    transmittances. Fixed, water, and ozone transmittances are each
!    checked individually to be sure they give 0 < trans < 1.

!    ===================================================================
!    Loops downward over all the layers for each of the NCHN7 channels
!    to compute the layer transmittances TAU.

!    The water continuum absorption coefficient is:
!       k_con = the sum i=1 to 5 of { COEF(i)*CONPRD(i) }

!    The layer effective fixed gas absorption coefficient is:
!       k_fixed = the sum i=1 to 8 of { COEF(5+i)*FPRED(i) }

!    The layer effective water lines absorption coefficient is:
!       k_water = the sum i=1 to 13 of { COEF(5+8+i)*WPRED(i) }

!    The layer effective ozone absorption coefficient is:
!       k_ozone = COEF(5+8+13+1)*OPRED(1)

!    where
!      "COEF" are the fast transmittance coefficients COEF4
!      "CONPRD" are the water continuum predictors CONPRD
!      "FPRED" are the fixed gases predictors FPRED
!      "WPRED" are the water lines predictors WPRED4
!      "OPRED" are the ozone predictors OPRED4

!    The total layer effective transmittance TAU is:
!       TAU = exp( -[ k_con + k_fixed + k_water + k_ozone ])
!    TAU is only calc'ed if LTAU is TRUE.

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
!  7 Jul 1997 Scott Hannon   Created for set7
!  3 Sep 1997 Scott Hannon   Added TAUZ and BLMULT
! 30 Sep 1997 Scott Hannon   Added variable CO2
! 27 Feb 1998 Scott Hannon   Added LTAU
! 11 Aug 2000 Scott Hannon   Change from 4 to 5 term H2O continuum
! 12 Sep 2002 Scott Hannon   Add predictors 6 & 7 to H2O con
!  3 Jan 2003 Scott Hannon   Add XZ
! 12 Oct 2004 Scott Hannon   Change CO2MLT from scaler to vector
! 28 Jun 2005 Scott Hannon   "trace" version for CO2,N2O
! 30 Apr 2008 Scott Hannon   Change CO2 from 4 to 5 predictors
! 1  Feb 2019 C Hepplewhite  Add HDO
!  1 Jun 2022 C Hepplewhite  Add s/w CH4 per Scott.

!END====================================================================

!      =================================================================

SUBROUTINE CALT7 ( LTAU, INDCHN, NLAY, BLMULT, NCHN7, CLIST7,  &
    COEF7,  FIXMUL, CONPD7, FPRED7, WPRED7, OPRED7, DPRED, TRCPRD,  &
    INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT,  &
INDHDO  COFHDO, HDOMLT, INDCH4, COFCH4, CH4MLT, XZ, TAU, TAUZ )
!      =================================================================
  
!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------
  
  LOGICAL, INTENT(IN OUT)                  :: LTAU
  INTEGER, INTENT(IN)                      :: INDCHN(MXCHAN)
  INTEGER, INTENT(IN)                      :: NLAY
  REAL, INTENT(IN)                         :: BLMULT
  INTEGER, INTENT(IN)                      :: NCHN7
  INTEGER, INTENT(IN)                      :: CLIST7(MXCHN7)
  REAL, INTENT(IN)                         :: COEF7(N7COEF,MAXLAY,MXCHN7)
  REAL, INTENT(IN)                         :: FIXMUL(MAXLAY)
  REAL, INTENT(IN)                         :: CONPD7( N7CON,MAXLAY)
  REAL, INTENT(IN)                         :: FPRED7( N7FIX,MAXLAY)
  REAL, INTENT(IN)                         :: WPRED7( N7H2O,MAXLAY)
  REAL, INTENT(IN)                         :: OPRED7(  N7O3,MAXLAY)
  REAL, INTENT(IN)                         :: DPRED(  NHDO MAXLAY)
    REAL, INTENT(IN)                         :: TRCPRD(NTRACE,MAXLAY)
    INTEGER, INTENT(IN)                      :: INDCO2(MXCHAN)
    REAL, INTENT(IN)                         :: COFCO2(  NCO2,MAXLAY,MXCHNC)
    REAL, INTENT(IN)                         :: CO2MLT(MAXLAY)
    INTEGER, INTENT(IN)                      :: INDN2O(MXCHAN)
    REAL, INTENT(IN)                         :: COFN2O(  NN2O,MAXLAY,MXCHNN)
    REAL, INTENT(IN)                         :: N2OMLT(MAXLAY)
    NO TYPE, INTENT(IN OUT)                  :: INDHDO  CO
      REAL, INTENT(IN)                         :: HDOMLT(MAXLAY)
      INTEGER, INTENT(IN)                      :: INDCH4(MXCHAN)
      REAL, INTENT(IN)                         :: COFCH4(  NCH4,MAXLAY,MXCHNM)
      REAL, INTENT(IN)                         :: CH4MLT(MAXLAY)
      REAL, INTENT(IN OUT)                     :: XZ
      REAL, INTENT(OUT)                        :: TAU(MAXLAY,MXCHAN)
      REAL, INTENT(OUT)                        :: TAUZ(MXCHAN)
      IMPLICIT NONE
      
      
!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
      INCLUDE 'incFTC.f'
      
      
!-----------------------------------------------------------------------
!      EXTERNAL FUNCTIONS
!-----------------------------------------------------------------------
!      QIKEXP
      
      
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
      INTEGER :: ICH4
      INTEGER :: ICO2
      INTEGER :: ILAY
      INTEGER :: IN2O
      INTEGER :: IHDO
        INTEGER :: J
        REAL :: DK
        REAL :: DKCO2
        REAL :: DKN2O
        REAL :: DKHDO
          REAL :: DKCH4
          REAL :: KHDO
            REAL :: KCON
            REAL :: KFIX
            REAL :: KOZO
            REAL :: KLAYER
            REAL :: KWAT
            REAL :: KZ
            LOGICAL :: LCO2
            LOGICAL :: LN2O
            LOGICAL :: LHDO
              LOGICAL :: LCH4
              
!      for function QIKEXP
              REAL :: QIKEXP
              
              
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
              DO I=1,NCHN7
                
!         Index for TAU
                J=INDCHN( CLIST7(I) )
                
!         Determine whether or not to do variable CO2
                ICO2=INDCO2( CLIST7(I) )
                IF (ICO2 > 0) THEN
                  LCO2=.TRUE.
                ELSE
                  LCO2=.FALSE.
                END IF
                
!         Determine whether or not to do variable CO2
                IN2O=INDN2O( CLIST7(I) )
                IF (IN2O > 0) THEN
                  LN2O=.TRUE.
                ELSE
                  LN2O=.FALSE.
                END IF
                
!         Determine whether or not to do variable HDO calc
                IHDO=INDHDO( CLIST7(I) )
                IF (IHDO > 0) THEN
                  LHDO=.TRUE.
                ELSE
                  LHDO=.FALSE.
                END IF
                
!         Determine whether or not to do variable CH4
                ICH4=INDCH4( CLIST7(I) )
                IF (ICH4 > 0) THEN
                  LCH4=.TRUE.
                ELSE
                  LCH4=.FALSE.
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
                  KCON=( COEF7(1,ILAY,I)*CONPD7(1,ILAY) ) +  &
                      ( COEF7(2,ILAY,I)*CONPD7(2,ILAY) ) +  &
                      ( COEF7(3,ILAY,I)*CONPD7(3,ILAY) ) +  &
                      ( COEF7(4,ILAY,I)*CONPD7(4,ILAY) ) +  &
                      ( COEF7(5,ILAY,I)*CONPD7(5,ILAY) ) +  &
                      ( COEF7(6,ILAY,I)*CONPD7(6,ILAY) ) +  &
                      ( COEF7(7,ILAY,I)*CONPD7(7,ILAY) )
                  
                  IF (KCON < 0.0E+0) THEN
                    KCON=0.0E+0
                  ELSE IF (KCON > 1.0E+1) THEN
                    KCON=1.0E+1
                  END IF
                  
!            -----------------------------
!            Calc the fixed gases abs coef
!            -----------------------------
                  KFIX=( COEF7( 8,ILAY,I)*FPRED7( 1,ILAY) ) +  &
                      ( COEF7( 9,ILAY,I)*FPRED7( 2,ILAY) ) +  &
                      ( COEF7(10,ILAY,I)*FPRED7( 3,ILAY) ) +  &
                      ( COEF7(11,ILAY,I)*FPRED7( 4,ILAY) ) +  &
                      ( COEF7(12,ILAY,I)*FPRED7( 5,ILAY) ) +  &
                      ( COEF7(13,ILAY,I)*FPRED7( 6,ILAY) ) +  &
                      ( COEF7(14,ILAY,I)*FPRED7( 7,ILAY) ) +  &
                      ( COEF7(15,ILAY,I)*FPRED7( 8,ILAY) )
                  
                  KFIX=KFIX*FIXMUL(ILAY)
                  
                  IF (KFIX < 0.0E+0) THEN
                    KFIX=0.0E+0
                  ELSE IF (KFIX > 1.0E+1) THEN
                    KFIX=1.0E+1
                  END IF
                  
                  
!            --------------------------
!            Compute the water abs coef
!            --------------------------
                  KWAT=( COEF7(16,ILAY,I)*WPRED7( 1,ILAY) ) +  &
                      ( COEF7(17,ILAY,I)*WPRED7( 2,ILAY) ) +  &
                      ( COEF7(18,ILAY,I)*WPRED7( 3,ILAY) ) +  &
                      ( COEF7(19,ILAY,I)*WPRED7( 4,ILAY) ) +  &
                      ( COEF7(20,ILAY,I)*WPRED7( 5,ILAY) ) +  &
                      ( COEF7(21,ILAY,I)*WPRED7( 6,ILAY) ) +  &
                      ( COEF7(22,ILAY,I)*WPRED7( 7,ILAY) ) +  &
                      ( COEF7(23,ILAY,I)*WPRED7( 8,ILAY) ) +  &
                      ( COEF7(24,ILAY,I)*WPRED7( 9,ILAY) ) +  &
                      ( COEF7(25,ILAY,I)*WPRED7(10,ILAY) ) +  &
                      ( COEF7(26,ILAY,I)*WPRED7(11,ILAY) ) +  &
                      ( COEF7(27,ILAY,I)*WPRED7(12,ILAY) ) +  &
                      ( COEF7(28,ILAY,I)*WPRED7(13,ILAY) )
                  
                  IF (KWAT < 0.0E+0) THEN
                    KWAT=0.0E+0
                  ELSE IF( KWAT > 1.0E+1) THEN
                    KWAT=1.0E+1
                  END IF
                  
                  
!            --------------------------
!            Compute the ozone abs coef
!            --------------------------
                  KOZO=( COEF7(29,ILAY,I)*OPRED7(1,ILAY) )
                  
                  IF (KOZO < 0.0E+0) THEN
                    KOZO=0.0E+0
                  ELSE IF (KOZO > 1.0E+1) THEN
                    KOZO=1.0E+1
                  END IF
                  
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
!     $               ( COFHDO(2,ILAY,IHDO)*DPRED( 2,ILAY) ) +
!     $               ( COFHDO(3,ILAY,IHDO)*DPRED( 3,ILAY) ) +
!     $               ( COFHDO(4,ILAY,IHDO)*DPRED( 4,ILAY) ) +
!     $               ( COFHDO(5,ILAY,IHDO)*DPRED( 5,ILAY) ) +
!     $               ( COFHDO(6,ILAY,IHDO)*DPRED( 6,ILAY) ) +
!     $               ( COFHDO(7,ILAY,IHDO)*DPRED( 7,ILAY) ) +
!     $               ( COFHDO(8,ILAY,IHDO)*DPRED( 8,ILAY) ) +
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
                  
!           kcon=0.0E+0
!           kfix=0.0E+0
!           kwat=0.0E+0
!           kozo=0.0E+0
!           KHDO=0.0E+0
!cccc
                  
!            ----------------------------
!            Calc change in total optical
!            depth due to variable CO2
!            ----------------------------
                  IF (LCO2 .AND. CO2MLT(ILAY) /= 0.0) THEN
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
!            depth due to variable N2O
!            ----------------------------
                  IF (LN2O .AND. N2OMLT(ILAY) /= 0.0) THEN
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
!            depth due to variable CH4
!            ----------------------------
                  IF (LCH4 .AND. CH4MLT(ILAY) /= 0.0) THEN
                    DKCH4=( COFCH4(1,ILAY,ICH4)*TRCPRD(1,ILAY) ) +  &
                        ( COFCH4(2,ILAY,ICH4)*TRCPRD(2,ILAY) ) +  &
                        ( COFCH4(3,ILAY,ICH4)*TRCPRD(3,ILAY) ) +  &
                        ( COFCH4(4,ILAY,ICH4)*TRCPRD(4,ILAY) ) +  &
                        ( COFCH4(5,ILAY,ICH4)*TRCPRD(5,ILAY) ) +  &
                        ( COFCH4(6,ILAY,ICH4)*TRCPRD(6,ILAY) ) +  &
                        ( COFCH4(7,ILAY,ICH4)*TRCPRD(7,ILAY) )
                    DKCH4=DKCH4*CH4MLT(ILAY)
                  ELSE
                    DKCH4=0.0
                  END IF
                  
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
                  
!cc
! this block for testing
!      DKCO2=0.0
!      DKN2O=0.0
!      DKHDO=0.0
!      DKCH4=0.0
                  IF (.NOT. CFHDO) KHDO=0.0
!cc
!            Limit -DK so it can never totally totally cancel KFIX
                  DK = DKCO2 + DKN2O + DKCH4
                  IF (-DK >= KFIX) THEN
                    DK = -0.999*KFIX
                  END IF
                  
!            Calc total layer optical depth
                  KLAYER=KCON + KFIX + KWAT + KOZO + KHDO + DK
                    
!            Adjust the optical depth of the bottom layer
                    IF (ILAY == NLAY) KLAYER=BLMULT*KLAYER
                    
!            Calc layer-to-space optical depth
                    KZ=KZ + KLAYER
                    
!            Calc effective layer transmittance
                    IF (LTAU) TAU(ILAY,J)=QIKEXP(-KLAYER)
                    
                    ENDDO
!         End loop on levels
                      
!         Convert KZ to TAUZ
                      TAUZ(J)=QIKEXP(-KZ*XZ)
                      
                      ENDDO
!      End loops on channel number (frequency)
                        
                        RETURN
                      END SUBROUTINE CALT7
