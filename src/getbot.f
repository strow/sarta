C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore County [UMBC]
C
C    AIRS
C
C    GETBOT
C
!F77====================================================================


!ROUTINE NAME:
C    GETBOT


!ABSTRACT:
C    Calculate the bottom layer number and fractional multiplier
C    based on the supplied surface pressure and temperature profile


!CALL PROTOCOL:
C    GETBOT( NLAY, PLEV, PSURF, LBOT, BLMULT )


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER   NLAY    number of profile layers    none
C    REAL arr  PLEV    layer pres level boundaries mb
C    REAL      PSURF   surface pressure            mb


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER   LBOT    bottom layer number         none
C    REAL      BLMULT  bot layer fractional mult   none


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
C    May 2001 version of the 100 layer AIRS Fast Transmittance
C    Code by L.L.Strow/S.Hannon/H.Motteler.
C
C    This routine starts at layer 100 and loops upward until it finds
C    the layer bounding PSURF.  It then computes the fraction of this
C    bottom layer above PSURF.  A bottom layer thinner than 5% of the
C    full layer thickness is avoided; in this case the layer directly
C    above is used instead with a fraction slightly larger than 1.
C
C    ===================================================================


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    Assumes the user has supplied vaguely realistic profile amounts
C    and temperatures.


!ROUTINE HISTORY:
C    Date        Programmer     Comments
C    ----------- -------------- --------------------------------------
C    31 Mar 2000 Scott Hannon   Created
C     1 May 2001 Scott Hannon   Add DELPX and check bottom thickness
C     2 May 2001 Scott Hannon   PLEV changed from local data to input
C    17 Dec 2004 Scott Hannon   Add NLAY to call; trap LBOT>NLAY;
C                               add warning for excessive BLMULT
C    24 Jun 2005 Scott Hannon   "10" loop changed to start on DELPX
C                               assignment rather than IF line below.


!END====================================================================

C      =================================================================
       SUBROUTINE GETBOT ( NLAY, PLEV, PSURF, LBOT, BLMULT )
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
       INTEGER   NLAY         ! number of profile layers
       REAL   PLEV(MAXLAY+1)  ! layer pressure level boundaries
       REAL  PSURF            ! surface pressure
C
C      Output
       INTEGER   LBOT         ! bottom layer number
       REAL BLMULT            ! bottom layer fractional multiplier


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER  LBOTX  ! unrestricted bottom layer number
       REAL  DELPX     ! 5% of layer thickness in pressure


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
C      Determine LBOT from PSURF by comparing to PLEV
       LBOTX=MAXLAY
 10    DELPX=0.05*( PLEV(LBOTX+1) - PLEV(LBOTX) )
       IF (PSURF .LT. PLEV(LBOTX)+DELPX) THEN
          LBOTX=LBOTX - 1
          GOTO 10
       ENDIF
C
       IF (LBOTX .GT. NLAY) THEN
          LBOT = NLAY
       ELSE
          LBOT = LBOTX
       ENDIF
C
C      Calc bottom layer multiplier (fractional layer)
       BLMULT = (PSURF - PLEV(LBOT))/(PLEV(LBOT+1) - PLEV(LBOT))
C
       IF (BLMULT .GT. 1.3) THEN
          WRITE(IOINFO,1010) BLMULT, LBOTX, NLAY
 1010     FORMAT('WARNING! excessive BLMULT=',F6.3,'; optimal LBOT=',
     $    I3,' but layers end at NLAY=',I3)
       ENDIF
C
       RETURN
       END
