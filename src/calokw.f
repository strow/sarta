C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore County [UMBC]
C
C    AIRS
C
C    CALOKW
C
!F77====================================================================


!ROUTINE NAME:
C    CALOKW


!ABSTRACT:
C    Calculate the OPTRAN derived water pressure layer effective
C    optical depth for a single channel.


!CALL PROTOCOL:
C    CALOKW ( LBOT, ICHAN, LOPMIN, LOPMAX, LOPLOW, LOPUSE,
C       H2OPRD, COFH2O, WAOP, DAOP, WAANG, KW )


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER   LBOT    bottom pres layer number    none
C    INTEGER   ICHAN   OPTRAN water channel index  none
C    INTEGER   LOPMIN  min OPTRAN level to use     none
C    INTEGER   LOPMAX  max OPTRAN level to use     none
C    INTEGER   LOPLOW  low OPTRAN bracketing lev   none
C    LOG arr   LOPUSE  Need this OPTRAN level?     none
C    REAL arr  H2OPRD  OPTRAN water predictors     various
C    REAL arr  COFH2O  OPTRAN H2O fast trans coef  various
C    REAL arr  WAOP    OPTRAN layer water amounts  kiloMoles/cm^2
C    REAL arr  DAOP    OPTRAN-to-AIRS interp fact  none
C    REAL arr  WAANG   AIRS layer water amounts    kiloMoles/cm^2


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    REAL arr  KW      AIRS H2O layer eff op dep   none


!INPUT/OUTPUT PARAMETERS:
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    CALT1


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
C
C    The OPTRAN predictors and fast transmittance coefficients are
C    used to calculate the water effective absorption coefficient on
C    on the OPTRAN level grid.  Only the OPTRAN levels actually needed
C    are calculated.
C    Note: the COFH2O*H2OPRD result must be divided by WAOP, a scaling
C    factor which was originally applied during the fast transmittance
C    coefficient regression.
C    The OPTRAN absorption coefficients are then interpolated onto the
C    100 AIRS layers and multiplied by the AIRS layer water amount (to
C    convert absorption coefficient into optical depth).


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date         Programmer      Comments
C    -----------  --------------  --------------------------------------
C    27 Feb 1998  Scott Hannon    Created
C    26 Aug 1998  Scott Hannon    Add LBOT to call; loop on LBOT instead
C                                 of MAXLAY


!END====================================================================

C      =================================================================
       SUBROUTINE CALOKW ( LBOT, ICHAN, LOPMIN, LOPMAX, LOPLOW, LOPUSE,
     $    H2OPRD, COFH2O, WAOP, DAOP, WAANG, KW )
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
       INTEGER  ICHAN
       INTEGER LOPMIN
       INTEGER LOPMAX
       INTEGER LOPLOW(MAXLAY)
       LOGICAL LOPUSE(MXOWLY)
       REAL   DAOP(MAXLAY)
       REAL   WAANG(MAXLAY)
       REAL   WAOP(MXOWLY)
       REAL  H2OPRD(  NH2O,MXOWLY)
       REAL  COFH2O(  NH2O,MXOWLY,MXCHNW)

C      Output
       REAL  KW(MAXLAY)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER      L
       INTEGER    LOP
       REAL   KWOP(MXOWLY)


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
C      ---------------------------------
C      Loop over the OPTRAN water levels
C      ---------------------------------
C      Only do calc for OPTRAN levels that are needed
       DO LOP=LOPMIN,LOPMAX
          IF (LOPUSE(LOP)) THEN
             KWOP(LOP)=
     $          COFH2O(1,LOP,ICHAN)*H2OPRD(1,LOP) +
     $          COFH2O(2,LOP,ICHAN)*H2OPRD(2,LOP) +
     $          COFH2O(3,LOP,ICHAN)*H2OPRD(3,LOP) +
     $          COFH2O(4,LOP,ICHAN)*H2OPRD(4,LOP) +
     $          COFH2O(5,LOP,ICHAN)*H2OPRD(5,LOP) +
     $          COFH2O(6,LOP,ICHAN)*H2OPRD(6,LOP) +
     $          COFH2O(7,LOP,ICHAN)*H2OPRD(7,LOP) +
     $          COFH2O(8,LOP,ICHAN)*H2OPRD(8,LOP) +
     $          COFH2O(9,LOP,ICHAN)*H2OPRD(9,LOP)
C            Remove WAOP scaling factor
             KWOP(LOP)=KWOP(LOP)/WAOP(LOP)
C            Check for negative value
             IF (KWOP(LOP) .LT. 0.0E+0) KWOP(LOP)=0.0E+0
          ENDIF
       ENDDO
C
C      -------------------------
C      Loop over the AIRS layers
C      -------------------------
       DO L=1,LBOT
C
C         Interpolate abs coef and convert to optical depth
          KW(L)=( DAOP(L)*( KWOP(LOPLOW(L) + 1) -
     $       KWOP(LOPLOW(L)) ) + KWOP(LOPLOW(L)) )*WAANG(L)
          IF (KW(L) .LT. 0.0E+0) KW(L)=0.0E+0
C
       ENDDO
C
       RETURN
       END
