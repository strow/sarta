!=======================================================================
 
! Code converted using TO_F90_LOOP by Alan Miller
! Date: 2023-04-04  Time: 16:44:55
 
!=======================================================================

!    University of Maryland Baltimore County [UMBC]

!    AIRS

!    CALT7 (set7=FWO sun mfbw) version for trace gases (no SO2 or HNO3)

!F77====================================================================


!ROUTINE NAME:
!    CALT7


!ABSTRACT:
!    Calculate the transmittance for set7 using the predictors and the
!    fast transmittance coefficients.


!CALL PROTOCOL:
!    CALT7( INDCHN, NLAY, NCHN7, CLIST7, COEF7,
!       FIXMUL, CONPD7, FPRED7, WPRED7, OPRED7, TRCPRD, INDCO2, COFCO2,
!       CO2MLT, INDN2O, COFN2O, N2OMLT, TAU, TAUZ )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INT arr   INDCHN  channel indices             none
!    INTEGER   NLAY    number of layers to bottom  none
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

!    The total layer effective optical depth TAU is:
!       TAU = [ k_con + k_fixed + k_water + k_ozone ]

!    ===================================================================


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    none


!ROUTINE HISTORY:
! Date        Programmer     Comments
! ----------- -------------- -------------------------------------------
! 07 Jul 1997 Scott Hannon   Created for set7
! 03 Sep 1997 Scott Hannon   Added TAUZ and BLMULT
! 30 Sep 1997 Scott Hannon   Added variable CO2
! 27 Feb 1998 Scott Hannon   Added LTAU
! 11 Aug 2000 Scott Hannon   Change from 4 to 5 term H2O continuum
! 12 Sep 2002 Scott Hannon   Add predictors 6 & 7 to H2O con
! 03 Jan 2003 Scott Hannon   Add XZ
! 12 Oct 2004 Scott Hannon   Change CO2MLT from scaler to vector
! 28 Jun 2005 Scott Hannon   "trace" version for CO2,N2O
! 28 Mar 2006 Scott Hannon   Change TAU from trans to optical depth
! 22 Dec 2006 Scott Hannon   Change TAUZ from trans to optical depth
!                            and from (1 x n) to (m x n) array;
!                            delete func QIKEXP & arguments BLMULT
!                            & LTAU & XZ.
! 02 Sep 2008 Scott Hannon   Add 5th CO2 predictor

!END====================================================================

!      =================================================================

SUBROUTINE XCALT7 ( INDCHN, NLAY, NCHN7, CLIST7,  &
    COEF7, FIXMUL, CONPD7, FPRED7, WPRED7, OPRED7, TRCPRD, INDCO2,  &
    COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT, TAU, TAUZ )
!      =================================================================

!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------

INTEGER, INTENT(IN)                      :: INDCHN(MXCHAN)
INTEGER, INTENT(IN)                      :: NLAY
INTEGER, INTENT(IN)                      :: NCHN7
INTEGER, INTENT(IN)                      :: CLIST7(MXCHN7)
REAL, INTENT(IN)                         :: COEF7(N7COEF,MAXLAY,MXCHN7)
REAL, INTENT(IN)                         :: FIXMUL(MAXLAY)
REAL, INTENT(IN)                         :: CONPD7( N7CON,MAXLAY)
REAL, INTENT(IN)                         :: FPRED7( N7FIX,MAXLAY)
REAL, INTENT(IN)                         :: WPRED7( N7H2O,MAXLAY)
REAL, INTENT(IN)                         :: OPRED7(  N7O3,MAXLAY)
REAL, INTENT(IN)                         :: TRCPRD(NTRACE,MAXLAY)
INTEGER, INTENT(IN)                      :: INDCO2(MXCHAN)
REAL, INTENT(IN)                         :: COFCO2(  NCO2,MAXLAY,MXCHNC)
REAL, INTENT(IN)                         :: CO2MLT(MAXLAY)
INTEGER, INTENT(IN)                      :: INDN2O(MXCHAN)
REAL, INTENT(IN)                         :: COFN2O(  NN2O,MAXLAY,MXCHNN)
REAL, INTENT(IN)                         :: N2OMLT(MAXLAY)
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


















!      Output




!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
INTEGER :: I
INTEGER :: ICO2
INTEGER :: ILAY
INTEGER :: IN2O
INTEGER :: J
REAL :: DK
REAL :: DKCO2
REAL :: DKN2O
REAL :: KCON
REAL :: KFIX
REAL :: KOZO
REAL :: KLAYER
REAL :: KWAT
REAL :: KZ
LOGICAL :: LCO2
LOGICAL :: LN2O


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
        ( COEF7(6,ILAY,I)*CONPD7(6,ILAY) ) + ( COEF7(7,ILAY,I)*CONPD7(7,ILAY) )
    
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
    
!cc
! this block for testing
!      DKCO2=0.0
!      DKN2O=0.0
!cc
!            Limit -DK so it can never totally totally cancel KFIX
    DK = DKCO2 + DKN2O
    IF (-DK >= KFIX) THEN
      DK = -0.999*KFIX
    END IF
    
!            Calc total layer optical depth
    KLAYER=KCON + KFIX + KWAT + KOZO + DK
    TAU(ILAY,J)=KLAYER
    
!            Calc layer-to-space optical depth
    KZ=KZ + KLAYER
    TAUZ(ILAY,J)=KZ
    
    ENDDO
!         End loop on levels
      
      ENDDO
!      End loops on channel number (frequency)
        
        RETURN
      END SUBROUTINE XCALT7
