!=======================================================================
!
!    University of Maryland Baltimore County [UMBC]
!
!    AIRS
!
!    CALNTE (for Non-local Thermodynamic Equilibrium)
!            extended solar angle range.
!F90====================================================================

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
!    REAL      SZALAY  solzen at layer1            degrees
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
!
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
!

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
!    Jun 2022 C Hepplewhite  Add extended range byond solzen>90 twilight.
!END====================================================================

!      =================================================================
   SUBROUTINE CALNTE ( INDCHN, TEMP, VSEC1, NCHNTE, CLISTN, &
       COEFN, CO2TOP, SUNANG, XALT, RAD )
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
!       REAL VACONV
real(4) :: SACONV

!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
!      Input
! INDCHN
! TEMP
! SUNANG ! solar zenith angle from prof.solzen.
! SZALAY ! solar zenith angle at layer1.
! SUNCOS ! solar zenith angle cosine at surface
! SCOS1 ! solar zenith angle cosine at layer1
! VSEC1 ! satellite view zenith angle secant at layer1
! XALT
! NCHNTE
! CLISTN
! COEFN
! CO2TOP ! CO2 mixing ratio in top layers (ppmv)
integer :: NCHNTE
integer, dimension(MXCNTE) :: CLISTN
integer, dimension(MXCHAN) :: INDCHN
real(4) :: SUNANG, SZALAY, SUNCOS, SCOS1, VSEC1, XALT, CO2TOP
real(4), dimension(XNCOEF,MXCNTE) :: COEFN
real(4), dimension(MAXLAY) :: TEMP
!
!      Input/Output
real(4), dimension(MXCHAN) :: RAD

!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
integer :: I, J
real(4) :: DRAD, PRED1, PRED2, PRED3, PRED4, PRED5, PRED6
real(4) :: THIGH, XZALAY, XUNCOS, XCOS1, NSUNANG

!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none

!***********************************************************************
!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!***********************************************************************
!

!      Recalculate SZALAY, SUNCOS, SCOS1 independent of sarta.f local
!      from SUNANG. Local vars prefix with X...
!      IF doing exended nonLTE Cap sunang to 120-deg.
!      IF doing 0-90 non LTE Cap sunang to 90-deg.
       IF(LXNTE) THEN
!          IF(SUNANG .GT. 120 ) NSUNANG = 120.0
!          IF(SUNANG .LE. 120 ) NSUNANG = SUNANG
         NSUNANG = MIN(SUNANG, 120.0);
       ELSEIF(.NOT. LXNTE) THEN
!          IF(SUNANG .GT. 90) NSUNANG = 90.0
!          IF(SUNANG .LE. 90) NSUNANG = SUNANG
         NSUNANG = MIN(SUNANG, 90.0);
       ENDIF
       XZALAY = SACONV(NSUNANG,XALT)    ! NSUNANG [deg] XZALAY [rad]
       XUNCOS = COS(DEG2RAD*NSUNANG)
       XCOS1 = COS(XZALAY)

!       write(6,"('calxnte: SUNANG,XALT,SZALAY',F6.2,X,F11.3,X,F11.3 )") 
!     $  SUNANG,XALT,XZALAY
!      Calculate the channel independent non-LTE predictors
!       THIGH = (TEMP(1) + TEMP(2) + TEMP(3) + TEMP(4) + TEMP(5))/5.0  ! airslay
!       THIGH = THIGH/250.0                            !/thb_mean from training 
       THIGH = (TEMP(2) + TEMP(3) + TEMP(4))/3.0                      ! PBL 
       PRED1 = 1.0
       PRED2 = XCOS1
       PRED3 = XCOS1*XCOS1
       PRED4 = XCOS1*VSEC1
       PRED5 = XCOS1*THIGH
       PRED6 = XUNCOS
!!       PRED2 = SCOS1
!!       PRED3 = SCOS1*SCOS1
!!       PRED4 = SCOS1*VSEC1
!!       PRED5 = SCOS1*THIGH
!!       PRED6 = SUNCOS

!      ---------------------------
!      Loop on channel (frequency)
!      ---------------------------
     DO I=1,NCHNTE
!
!         Index for RAD
        J=INDCHN( CLISTN(I) )
!
        IF(NSUNANG .LE. 90) THEN
          DRAD=( COEFN(1,I)*PRED1 ) + &
               ( COEFN(2,I)*PRED2 ) + &
               ( COEFN(3,I)*PRED3 ) + &
               ( COEFN(4,I)*PRED4 ) + &
               ( COEFN(5,I)*PRED5 ) + &
               ( COEFN(6,I)*PRED6 )
        ELSEIF(NSUNANG .GT. 90 .AND. LXNTE) THEN
          DRAD=( COEFN(8,I)*PRED1 ) + &
               ( COEFN(9,I)*PRED2 ) + &
               ( COEFN(10,I)*PRED3 ) + &
               ( COEFN(11,I)*PRED4 ) + &
               ( COEFN(12,I)*PRED5 ) + &
               ( COEFN(13,I)*PRED6 )
        ELSEIF(NSUNANG .GT. 90 .AND. .NOT. LXNTE) THEN
          DRAD = 0.0
        ENDIF
!
!       Adjust DRAD for CO2 mixing ratio
        DRAD=DRAD*(COEFN(7,I)*(CO2TOP - CO2NTE) + 1.0)
! DEBUG checking
!        DRAD=0.0
!
!       Adjust RAD for the non-LTE contribution
        RAD(J) = RAD(J) + DRAD/1000.0 ! convert DRAD to Watts
!
!       write(6,'(A,X,I3,X,E11.3)') 'calnte: I, XCO2 ',I,(COEFN(7,I)*(CO2TOP - CO2NTE) + 1.0)
!
     ENDDO
!    End loops on channel number (frequency)
!
     RETURN
   END
