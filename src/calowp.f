c This version uses amounts at center of layer, not lower boundary
C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore County [UMBC]
C
C    AIRS
C
C    CALOWP
C
!F77====================================================================


!ROUTINE NAME:
C    CALOWP


!ABSTRACT:
C    Calculate the OPTRAN water (H2O) predictors for a profile.


!CALL PROTOCOL:
C    CALOWP ( LBOT, WAMNT, P, T, SECANG, WAZOP, WAVGOP,
C       WAANG, LOPMIN, LOPMAX, LOPUSE, H2OPRD, LOPLOW, DAOP )


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER   LBOT    bottom layer number         none
C    REAL arr  WAMNT   profile layer water         kiloMoles/cm^2
C    REAL arr  P       layer pressures             atmospheres
C    REAL arr  T       profile temperature         K
C    REAL arr  SECANG  secant of path angle        none
C    REAL arr  WAZOP   OPTRAN l-to-s water grid    kiloMoles/cm^2
C    REAL arr  WAVGOP  OPTRAN average preds        various


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    REAL arr  WAANG   water amount in layer       kiloMoles/cm^2
C    INTEGER   LOPMIN  min OPTRAN level to use     none
C    INTEGER   LOPMAX  max OPTRAN level to use     none
C    LOG arr   LOPUSE  OPTRAN level needed?        none
C    REAL arr  H2OPRD  OPTRAN predictors           various
C    INTEGER   LOPLOW  low bracketing OPTRAN lev   none
C    REAL arr  DAOP    OPTRAN-to-AIRS interp frac  none


!INPUT/OUTPUT PARAMETERS:
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    USEFAST


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    incFTC.f : include file of parameter statements accessed during
C       compilation only.


!COMMON BLOCKS
C    none


!DESCRIPTION:
C    March 1998 version of the 100 layer AIRS Fast Transmittance
C    Code by L.L.Strow/S.Hannon.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    Assumes the user has supplied vaguely realistic profile amounts
C    and temperatures.


!ROUTINE HISTORY:
C    Date         Programmer      Comments
C    -----------  --------------  --------------------------------------
C    27 Feb 1998  Scott Hannon    Created
C    26 Aug 1998  Scott Hannon    Add LBOT to call; loop on LBOT instead
C                                 of MAXLAY


!END====================================================================

C      =================================================================
       SUBROUTINE CALOWP ( LBOT, WAMNT, P, T, SECANG, WAZOP, WAVGOP,
     $    WAANG, LOPMIN, LOPMAX, LOPUSE, H2OPRD, LOPLOW, DAOP )
C      =================================================================

C-----------------------------------------------------------------------
C      IMPLICIT NONE
C-----------------------------------------------------------------------
       IMPLICIT NONE


C-----------------------------------------------------------------------
C      INCLUDE FILES
C-----------------------------------------------------------------------
       include 'incFTC.f'


C-----------------------------------------------------------------------
C      EXTERNAL FUNCTIONS
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      Input
       INTEGER   LBOT
       REAL  WAMNT(MAXLAY)
       REAL      P(MAXLAY)
       REAL      T(MAXLAY)
       REAL SECANG(MAXLAY)
       REAL  WAZOP(MXOWLY)
       REAL WAVGOP(NOWAVG,MXOWLY)
C
C      Output
       REAL  WAANG(MAXLAY)
       INTEGER LOPMIN
       INTEGER LOPMAX
       REAL H2OPRD(  NH2O,MXOWLY)
       LOGICAL LOPUSE(MXOWLY)
       INTEGER LOPLOW(MAXLAY)
       REAL  DAOP(MAXLAY)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER      L
       INTEGER     LL
       INTEGER    LOP
       INTEGER   LOPL
       INTEGER   LOPU
       INTEGER     LU
       REAL    WAZ(MAXLAY)
       REAL WAZSUM
       REAL WPZSUM
       REAL WTZSUM
       REAL     PZ(MAXLAY)
       REAL     TZ(MAXLAY)
       REAL     DA
       REAL    POP
       REAL    TOP
       REAL   PZOP
       REAL   TZOP
       REAL  ANGOP
       LOGICAL   LAST


C-----------------------------------------------------------------------
C      SAVE STATEMENTS
C-----------------------------------------------------------------------
C      none


C***********************************************************************
C***********************************************************************
C                    EXECUTABLE CODE
C***********************************************************************
C***********************************************************************
C
C      Initialize amount above sums
       WAZSUM=0.0E+0
       WPZSUM=0.0E+0
       WTZSUM=0.0E+0
C
C      ---------------------------------------
C      Calculate raw predictors for all layers
C      ---------------------------------------
       DO L=1,LBOT
C
C         Layer amount*angle
          WAANG(L)=WAMNT(L)*SECANG(L)
C
C         Center-of-layer-to-space amount*angle
C         Note: do this before updating AZSUM
          WAZ(L)=5.0E-1*WAANG(L) + WAZSUM
C
C         Bottom-of-layer-to-space amount sum
          WAZSUM=WAANG(L) + WAZSUM
C
C         Pressure above sum
          WPZSUM=WAANG(L)*P(L) + WPZSUM
          PZ(L)=WPZSUM/WAZSUM
C
C         Temperature above sum
          WTZSUM=WAANG(L)*T(L) + WTZSUM
          TZ(L)=WTZSUM/WAZSUM
C
       ENDDO
C
C      --------------------------------------------------
C      Find the max OPTRAN level that is less than WAZ(1)
C      --------------------------------------------------
       LOPMIN=1
 30    IF (WAZOP(LOPMIN+1) .LT. WAZ(1)) THEN
          LOPMIN=LOPMIN + 1
          GOTO 30
       ENDIF
C
C      Initialize the upper and lower (pressure) layer index
       LL=1
       LU=2
       LAST=.FALSE.
C
C      ----------------------------------------
C      Loop over the OPTRAN layers (while loop)
C      ----------------------------------------
       LOP=LOPMIN
 10    IF (LOP .LE. MXOWLY) THEN
C
C         --------------------------------------------------------
C         Find the two pressure layers closest to the OPTRAN layer
C         --------------------------------------------------------
 20       IF (WAZ(LU) .LT. WAZOP(LOP)) THEN
             IF (LU .LT. LBOT) THEN
                LL=LU
                LU=LU + 1
                GOTO 20
             ELSE
                LAST=.TRUE.
             ENDIF
          ENDIF
C
C         Compute the interpolation fractor
          DA=(WAZOP(LOP) - WAZ(LL))/(WAZ(LU) - WAZ(LL))
C
C         Do the interpolation
          POP=( DA*(  P(LU) -  P(LL) ) +  P(LL) )/WAVGOP(1,LOP)
          TOP=( DA*(  T(LU) -  T(LL) ) +  T(LL) )/WAVGOP(2,LOP)
          PZOP=( DA*( PZ(LU) - PZ(LL) ) + PZ(LL) )/WAVGOP(3,LOP)
          TZOP=( DA*( TZ(LU) - TZ(LL) ) + TZ(LL) )/WAVGOP(4,LOP)
          ANGOP=DA*( SECANG(LU) - SECANG(LL) ) + SECANG(LL)
C
C         Assign the predictors
          H2OPRD(1,LOP)=1.0E+0
          H2OPRD(2,LOP)=POP
          H2OPRD(3,LOP)=TOP
          H2OPRD(4,LOP)=SQRT( POP )
          H2OPRD(5,LOP)=TOP**2
          H2OPRD(6,LOP)=POP*TOP
          H2OPRD(7,LOP)=ANGOP
          H2OPRD(8,LOP)=PZOP
          H2OPRD(9,LOP)=TZOP
C
C         Update LOP and loop
          IF (LAST .EQ. .TRUE.) THEN
             LOPMAX=LOP
C            Set LOP > MXOWLY to exit loop over LOP
             LOP=MXOWLY + 1
          ELSE
             LOP=LOP + 1
          ENDIF
          GOTO 10
C
       ENDIF
C      End while loop over LOP
C
C      -----------------
C      Initialize LOPUSE
C      -----------------
       DO LOP=1,MXOWLY
          LOPUSE(LOP)=.FALSE.
       ENDDO
C
C      ---------------------------------------
C      Determine what OPTRAN layers are needed
C      ---------------------------------------
C      Initialize LOPL and LOPU
       LOPL=LOPMIN
       LOPU=LOPMIN + 1
C
C      Loop over the AIRS pressure layers
       DO L=1,LBOT
C         Find the two OPTRAN levels that bracket the AIRS layer
 40       IF (WAZOP(LOPU) .LT. WAZ(L) .AND. LOPU .LT. LOPMAX) THEN
             LOPL=LOPU
             LOPU=LOPU + 1
             GOTO 40
          ENDIF
C
          LOPUSE(LOPL)=.TRUE.
          LOPUSE(LOPU)=.TRUE.
C         Assign the lower OPTRAN level
          LOPLOW(L)=LOPL
C         Assign the interpolation fraction
          DAOP(L)=(WAZ(L) - WAZOP(LOPL))/(WAZOP(LOPU) - WAZOP(LOPL))
       ENDDO
C
       RETURN
       END
