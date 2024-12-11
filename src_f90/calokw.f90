!=======================================================================
!
!    University of Maryland Baltimore County [UMBC]
!
!    AIRS
!
!    CALOKW
!
!F90====================================================================


!ROUTINE NAME:
!    CALOKW


!ABSTRACT:
!    Calculate the OPTRAN derived water pressure layer effective
!    optical depth for a single channel.


!CALL PROTOCOL:
!    CALOKW ( LBOT, ICHAN, LOPMIN, LOPMAX, LOPLOW, LOPUSE,
!       H2OPRD, COFH2O, WAOP, DAOP, WAANG, KW )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INTEGER   LBOT    bottom pres layer number    none
!    INTEGER   ICHAN   OPTRAN water channel index  none
!    INTEGER   LOPMIN  min OPTRAN level to use     none
!    INTEGER   LOPMAX  max OPTRAN level to use     none
!    INTEGER   LOPLOW  low OPTRAN bracketing lev   none
!    LOG arr   LOPUSE  Need this OPTRAN level?     none
!    REAL arr  H2OPRD  OPTRAN water predictors     various
!    REAL arr  COFH2O  OPTRAN H2O fast trans coef  various
!    REAL arr  WAOP    OPTRAN layer water amounts  kiloMoles/cm^2
!    REAL arr  DAOP    OPTRAN-to-AIRS interp fact  none
!    REAL arr  WAANG   AIRS layer water amounts    kiloMoles/cm^2


!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL arr  KW      AIRS H2O layer eff op dep   none


!INPUT/OUTPUT PARAMETERS:
!    none


!RETURN VALUES:
!    none


!PARENT(S):
!    CALT1


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
!
!    The OPTRAN predictors and fast transmittance coefficients are
!    used to calculate the water effective absorption coefficient on
!    on the OPTRAN level grid.  Only the OPTRAN levels actually needed
!    are calculated.
!    Note: the COFH2O*H2OPRD result must be divided by WAOP, a scaling
!    factor which was originally applied during the fast transmittance
!    coefficient regression.
!    The OPTRAN absorption coefficients are then interpolated onto the
!    100 AIRS layers and multiplied by the AIRS layer water amount (to
!    convert absorption coefficient into optical depth).


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    none


!ROUTINE HISTORY:
!    Date         Programmer      Comments
!    -----------  --------------  --------------------------------------
!    27 Feb 1998  Scott Hannon    Created
!    26 Aug 1998  Scott Hannon    Add LBOT to call; loop on LBOT instead
!                                 of MAXLAY


!END====================================================================

!      =================================================================
       SUBROUTINE CALOKW ( LBOT, ICHAN, LOPMIN, LOPMAX, LOPLOW, &
        LOPUSE, H2OPRD, COFH2O, WAOP, DAOP, WAANG, KW )
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

!      Output
       REAL  KW(MAXLAY)


!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
       INTEGER      L
       INTEGER    LOP
       REAL   KWOP(MXOWLY)


!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none

!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!
!      ---------------------------------
!      Loop over the OPTRAN water levels
!      ---------------------------------
!$$$       IF (DEBUG)  write(6,'(A,I4, X, I4)') 'calokw: LOPMIN, LOPMAX ', 
!$$$     $      LOPMIN, LOPMAX
!      Only do calc for OPTRAN levels that are needed
       DO LOP=LOPMIN,LOPMAX
          IF (LOPUSE(LOP)) THEN
             KWOP(LOP)= &
               COFH2O(1,LOP,ICHAN)*H2OPRD(1,LOP) + &
               COFH2O(2,LOP,ICHAN)*H2OPRD(2,LOP) + &
               COFH2O(3,LOP,ICHAN)*H2OPRD(3,LOP) + &
               COFH2O(4,LOP,ICHAN)*H2OPRD(4,LOP) + &
               COFH2O(5,LOP,ICHAN)*H2OPRD(5,LOP) + &
               COFH2O(6,LOP,ICHAN)*H2OPRD(6,LOP) + &
               COFH2O(7,LOP,ICHAN)*H2OPRD(7,LOP) + &
               COFH2O(8,LOP,ICHAN)*H2OPRD(8,LOP) + &
               COFH2O(9,LOP,ICHAN)*H2OPRD(9,LOP)
!            Remove WAOP scaling factor
             KWOP(LOP)=KWOP(LOP)/WAOP(LOP)
!            Check for negative value
             IF (KWOP(LOP) .LT. 0.0E+0) KWOP(LOP)=0.0E+0
          ENDIF
       ENDDO
!
!      -------------------------
!      Loop over the AIRS layers
!      -------------------------
       DO L=1,LBOT
!
!CC       catch bug: KWOP(0)
          IF (LOPLOW(L) .GT. 0.0E+0) THEN
          
!         Interpolate abs coef and convert to optical depth
          KW(L)=( DAOP(L)*( KWOP(LOPLOW(L) + 1) - &
            KWOP(LOPLOW(L)) ) + KWOP(LOPLOW(L)) )*WAANG(L)
          IF (KW(L) .LT. 0.0E+0) KW(L)=0.0E+0
!
       ENDIF
       ENDDO
!
       RETURN
       END
