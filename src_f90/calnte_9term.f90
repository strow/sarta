!=======================================================================
 
! Code converted using TO_F90_LOOP by Alan Miller
! Date: 2023-04-04  Time: 16:44:55
 
!=======================================================================

!    University of Maryland Baltimore County [UMBC]

!    AIRS

!    CALNTE (for Non-local Thermodynamic Equilibrium)

!F77====================================================================


!ROUTINE NAME:
!    CALNTE


!ABSTRACT:
!    Adjust a LTE atmospheric radiance for a non-LTE upper atmosphere.


!CALL PROTOCOL:
!    CALNTE( INDCHN, TEMP, SUNCOS, SCOS1, VSEC1,
!       NCHNTE, CLISTN, COEFN, CO2TOP, RAD)


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INT arr   INDCHN  channel indices             none
!    REAL arr  TEMP    temperature profile         Kelvin
!    REAL      SUNCOS  solzen cosine at surface    none
!    REAL      SCOS1   solzen cosine at layer1     none
!    REAL      VSEC1   satzen secant at layer1     none
!    INTEGER   NCHNTE  number of non-LTE channels  none
!    INT arr   CLISTN  non-LTE channel list        none
!    REAL arr  COEFN   non-LTE coefficients        various
!    REAL arr  CO2TOP  top layers CO2 mixing ratio ppmv


!OUTPUT PARAMETERS:
!    none


!INPUT/OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL arr  RAD     radiance                    W/(m^2.str.cm^-1)


!RETURN VALUES:
!    none


!PARENT(S):
!    SARTA


!ROUTINES CALLED:
!    none


!FILES ACCESSED:
!    incFTC.f : include file of parameter statements accessed during
!       compilation only.


!COMMON BLOCKS
!    none


!DESCRIPTION:
!    May 2008 version of the 100 layer AIRS Fast Transmittance
!    Code by L.L.Strow/S.Hannon.

!    In the upper atmosphere where the air is thin, the strong CO2
!    absoption bands in the 4 um region can absorb solar radiance
!    faster than collisons with other air molecules can re-distribute
!    the energy. The CO2 is no longer in thermodynamic equilibrium
!    with its surroundings, which results in a change to the CO2
!    vibrational band population statistics and its effective
!    radiating temperature.  This code applies a regression based
!    adjustment to the input radiance to account for non-LTE effects.
!    Coefficients and predictors are multiplied together and summed
!    to calculate the change in radiance for non-LTE conditions.


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    none


!ROUTINE HISTORY:
! Date        Programmer     Comments
! ----------- -------------- ------------------------------------------
! 15 Mar 2005 Scott Hannon   Created
! 13 Oct 2005 Scott Hannon   MXCHNN renamed MXCNTE to avoid conflict
!                               with MXCHNN used with N2O
! 14 May 2008 Scott Hannon   Add CO2 adjustment using 7th coef; pass in
!                               CO2TOP

!END====================================================================

!      =================================================================

SUBROUTINE CALNTE ( INDCHN, TEMP, SUNCOS, SCOS1, VSEC1,  &
    NCHNTE, CLISTN, COEFN, CO2TOP, RAD )
!      =================================================================

!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------

INTEGER, INTENT(IN)                      :: INDCHN(MXCHAN)
REAL, INTENT(IN)                         :: TEMP(MAXLAY)
REAL, INTENT(IN)                         :: SUNCOS
REAL, INTENT(IN)                         :: SCOS1
REAL, INTENT(IN)                         :: VSEC1
INTEGER, INTENT(IN)                      :: NCHNTE
INTEGER, INTENT(IN)                      :: CLISTN(MXCNTE)
REAL, INTENT(IN)                         :: COEFN(NNCOEF,MXCNTE)
NO TYPE, INTENT(IN OUT)                  :: CO2TOP
REAL, INTENT(OUT)                        :: RAD(MXCHAN)
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








REAL :: CO2TOP ! CO2 mixing ratio in top layers (ppmv)

!      Input/Output



!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
INTEGER :: I
INTEGER :: J
REAL :: DRAD
REAL :: PRED1
REAL :: PRED2
REAL :: PRED3
REAL :: PRED4
REAL :: PRED5
REAL :: PRED6
REAL :: THIGH


!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none


!***********************************************************************
!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!***********************************************************************

!      Calculate the channel independent non-LTE predictors
THIGH = (TEMP(1) + TEMP(2) + TEMP(3) + TEMP(4) + TEMP(5))/5.0
PRED1 = 1.0
PRED2 = SCOS1
PRED3 = SCOS1*SCOS1
PRED4 = SCOS1*VSEC1
PRED5 = SCOS1*THIGH
PRED6 = SUNCOS

!      ---------------------------
!      Loop on channel (frequency)
!      ---------------------------
DO I=1,NCHNTE
  
!         Index for RAD
  J=INDCHN( CLISTN(I) )
  
  DRAD=( COEFN(1,I)*PRED1 ) + ( COEFN(2,I)*PRED2 ) +  &
      ( COEFN(3,I)*PRED3 ) + ( COEFN(4,I)*PRED4 ) +  &
      ( COEFN(5,I)*PRED5 ) + ( COEFN(6,I)*PRED6 )
  
!         Adjust DRAD for CO2 mixing ratio
  DRAD=DRAD*(COEFN(7,I)*(CO2TOP - CO2NTE) + 1.0)
  
!         Adjust RAD for the non-LTE contribution
  RAD(J) = RAD(J) + DRAD/1000.0 ! convert DRAD to Watts
  
  WRITE(6,'(A,X,I3,X,E11.3)') 'calnte: I, XCO2 ',I,(COEFN(7,I)*(CO2TOP - CO2NTE) + 1.0)
  
  ENDDO
!      End loops on channel number (frequency)
    
    RETURN
  END SUBROUTINE CALNTE
