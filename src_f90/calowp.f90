!=======================================================================
!
!    University of Maryland Baltimore County [UMBC]
!
!    AIRS
!
!    CALOWP
!
!F90====================================================================


!ROUTINE NAME:
!    CALOWP


!ABSTRACT:
!    Calculate the OPTRAN water (H2O) predictors for a profile.


!CALL PROTOCOL:
!    CALOWP ( LBOT, WAMNT, P, T, SECANG, WAZOP, WAVGOP,
!       WAANG, LOPMIN, LOPMAX, LOPUSE, H2OPRD, LOPLOW, DAOP )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INTEGER   LBOT    bottom layer number         none
!    REAL arr  WAMNT   profile layer water         kiloMoles/cm^2
!    REAL arr  P       layer pressures             atmospheres
!    REAL arr  T       profile temperature         K
!    REAL arr  SECANG  secant of path angle        none
!    REAL arr  WAZOP   OPTRAN l-to-s water grid    kiloMoles/cm^2
!    REAL arr  WAVGOP  OPTRAN average preds        various


!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL arr  WAANG   water amount in layer       kiloMoles/cm^2
!    INTEGER   LOPMIN  min OPTRAN level to use     none
!    INTEGER   LOPMAX  max OPTRAN level to use     none
!    LOG arr   LOPUSE  OPTRAN level needed?        none
!    REAL arr  H2OPRD  OPTRAN predictors           various
!    INTEGER   LOPLOW  low bracketing OPTRAN lev   none
!    REAL arr  DAOP    OPTRAN-to-AIRS interp frac  none


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
!    March 1998 version of the 100 layer AIRS Fast Transmittance
!    Code by L.L.Strow/S.Hannon.


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    Assumes the user has supplied vaguely realistic profile amounts
!    and temperatures.


!ROUTINE HISTORY:
! This version uses amounts at center of layer, not lower boundary
!    Date         Programmer      Comments
!    -----------  --------------  --------------------------------------
!    27 Feb 1998  Scott Hannon    Created
!    26 Aug 1998  Scott Hannon    Add LBOT to call; loop on LBOT instead
!                                 of MAXLAY


!END====================================================================

!      =================================================================
       SUBROUTINE CALOWP ( LBOT, WAMNT, P, T, SECANG, WAZOP, WAVGOP, &
         WAANG, LOPMIN, LOPMAX, LOPUSE, H2OPRD, LOPLOW, DAOP )
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
!      none

!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
!      Input
integer ::   LBOT
real(4), dimension(MAXLAY) :: WAMNT,P,T,SECANG
real(4), dimension(MXOWLY) :: WAZOP
real(4), dimension(NOWAVG,MXOWLY) :: WAVGOP
!
!      Output
real(4), dimension(MAXLAY) ::  WAANG, DAOP
integer LOPMIN, LOPMAX
real(4), dimension(NH2O,MXOWLY) :: H2OPRD
logical, dimension(MXOWLY) :: LOPUSE
integer, dimension(MAXLAY) :: LOPLOW


!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
integer ::  L, LL, LOP, LOPL, LOPU, LU
real, dimension(MAXLAY) :: WAZ, PZ, TZ
real(4) :: WAZSUM, WPZSUM, WTZSUM, DA, POP, TOP, PZOP, TZOP, ANGOP
logical ::  LAST


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
!      Initialize amount above sums
       WAZSUM=0.0E+0
       WPZSUM=0.0E+0
       WTZSUM=0.0E+0
!
!      ---------------------------------------
!      Calculate raw predictors for all layers
!      ---------------------------------------
       DO L=1,LBOT
!
!         Layer amount*angle
          WAANG(L)=WAMNT(L)*SECANG(L)
!
!         Center-of-layer-to-space amount*angle
!         Note: do this before updating AZSUM
          WAZ(L)=5.0E-1*WAANG(L) + WAZSUM
!
!         Bottom-of-layer-to-space amount sum
          WAZSUM=WAANG(L) + WAZSUM
!
!         Pressure above sum
          WPZSUM=WAANG(L)*P(L) + WPZSUM
          PZ(L)=WPZSUM/WAZSUM
!
!         Temperature above sum
          WTZSUM=WAANG(L)*T(L) + WTZSUM
          TZ(L)=WTZSUM/WAZSUM
!
!          write(6,'(A,I3,X,E11.4,X,E11.4)') 'calowp:WAZ(L),WAZSUM : ',L,WAZ(L),WAZSUM
       ENDDO
!
       if (DEBUG) print*,'calowp: completed raw predictors'
!      --------------------------------------------------
!      Find the max OPTRAN level that is less than WAZ(1)
!      --------------------------------------------------
       LOPMIN=1
       if (DEBUG) WRITE(6,'(A, E11.4)') 'calowp: WAZ(1) = ',WAZ(1)
 30    IF (WAZOP(LOPMIN+1) .LT. WAZ(1)) THEN
          LOPMIN=LOPMIN + 1
          GOTO 30
       ENDIF
!
!      Initialize the upper and lower (pressure) layer index
       LL=1
       LU=2
       LAST=.FALSE.
!
       if (DEBUG) print*,'calowp: completed find max optran level'
!      ----------------------------------------
!      Loop over the OPTRAN layers (while loop)
!      ----------------------------------------
       LOP=LOPMIN
 10    IF (LOP .LE. MXOWLY) THEN
!
!         --------------------------------------------------------
!         Find the two pressure layers closest to the OPTRAN layer
!         --------------------------------------------------------
 20       IF (WAZ(LU) .LT. WAZOP(LOP)) THEN
             IF (LU .LT. LBOT) THEN
                LL=LU
                LU=LU + 1
                GOTO 20
             ELSE
                LAST=.TRUE.
             ENDIF
          ENDIF
!
!         Compute the interpolation fractor
          DA=(WAZOP(LOP) - WAZ(LL))/(WAZ(LU) - WAZ(LL))
!
!         Do the interpolation
          POP=( DA*(  P(LU) -  P(LL) ) +  P(LL) )/WAVGOP(1,LOP)
          TOP=( DA*(  T(LU) -  T(LL) ) +  T(LL) )/WAVGOP(2,LOP)
          PZOP=( DA*( PZ(LU) - PZ(LL) ) + PZ(LL) )/WAVGOP(3,LOP)
          TZOP=( DA*( TZ(LU) - TZ(LL) ) + TZ(LL) )/WAVGOP(4,LOP)
          ANGOP=DA*( SECANG(LU) - SECANG(LL) ) + SECANG(LL)
!
!         Assign the predictors
          H2OPRD(1,LOP)=1.0E+0
          H2OPRD(2,LOP)=POP
          H2OPRD(3,LOP)=TOP
          H2OPRD(4,LOP)=SQRT( POP )
          H2OPRD(5,LOP)=TOP**2
          H2OPRD(6,LOP)=POP*TOP
          H2OPRD(7,LOP)=ANGOP
          H2OPRD(8,LOP)=PZOP
          H2OPRD(9,LOP)=TZOP
!
!         Update LOP and loop
          IF (LAST .EQV. .TRUE.) THEN
             LOPMAX=LOP
!            Set LOP > MXOWLY to exit loop over LOP
             LOP=MXOWLY + 1
          ELSE
             LOP=LOP + 1
          ENDIF
          GOTO 10
!
       ENDIF
!      End while loop over LOP
!
!      -----------------
!      Initialize LOPUSE
!      -----------------
       DO LOP=1,MXOWLY
          LOPUSE(LOP)=.FALSE.
       ENDDO
!
!      ---------------------------------------
!      Determine what OPTRAN layers are needed
!      ---------------------------------------
!      Initialize LOPL and LOPU
       LOPL=LOPMIN
       LOPU=LOPMIN + 1
!
!      Loop over the AIRS pressure layers
       DO L=1,LBOT
!         Find the two OPTRAN levels that bracket the AIRS layer
 40       IF (WAZOP(LOPU) .LT. WAZ(L) .AND. LOPU .LT. LOPMAX) THEN
             LOPL=LOPU
             LOPU=LOPU + 1
             GOTO 40
          ENDIF
!
          LOPUSE(LOPL)=.TRUE.
          LOPUSE(LOPU)=.TRUE.
!         Assign the lower OPTRAN level
          LOPLOW(L)=LOPL
!         Assign the interpolation fraction
          DAOP(L)=(WAZ(L) - WAZOP(LOPL))/(WAZOP(LOPU) - WAZOP(LOPL))
       ENDDO
!
       RETURN
       END
