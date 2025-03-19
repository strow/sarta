!=======================================================================
!
!    University of Maryland Baltimore County [UMBC]
!
!    AIRS
!
!    GETBOT
!
!F90====================================================================


!ROUTINE NAME:
!    GETBOT


!ABSTRACT:
!    Calculate the bottom layer number and fractional multiplier
!    based on the supplied surface pressure and temperature profile


!CALL PROTOCOL:
!    GETBOT( NLAY, PLEV, PSURF, LBOT, BLMULT )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INTEGER   NLAY    number of profile layers    none
!    REAL arr  PLEV    layer pres level boundaries mb
!    REAL      PSURF   surface pressure            mb


!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INTEGER   LBOT    bottom layer number         none
!    REAL      BLMULT  bot layer fractional mult   none


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
!    May 2001 version of the 100 layer AIRS Fast Transmittance
!    Code by L.L.Strow/S.Hannon/H.Motteler.
!
!    This routine starts at layer 100 and loops upward until it finds
!    the layer bounding PSURF.  It then computes the fraction of this
!    bottom layer above PSURF.  A bottom layer thinner than 5% of the
!    full layer thickness is avoided; in this case the layer directly
!    above is used instead with a fraction slightly larger than 1.
!
!    ===================================================================


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    Assumes the user has supplied vaguely realistic profile amounts
!    and temperatures.


!ROUTINE HISTORY:
!    Date        Programmer     Comments
!    ----------- -------------- --------------------------------------
!    31 Mar 2000 Scott Hannon   Created
!     1 May 2001 Scott Hannon   Add DELPX and check bottom thickness
!     2 May 2001 Scott Hannon   PLEV changed from local data to input
!    17 Dec 2004 Scott Hannon   Add NLAY to call; trap LBOT>NLAY;
!                               add warning for excessive BLMULT
!    24 Jun 2005 Scott Hannon   "10" loop changed to start on DELPX
!                               assignment rather than IF line below.


!END====================================================================

!      =================================================================
       SUBROUTINE GETBOT ( NLAY, PLEV, PSURF, LBOT, BLMULT )
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
integer ::  NLAY                     ! number of profile layers
real(4), dimension(MAXLAY+1) :: PLEV ! layer pressure level boundaries
real(4) :: PSURF                     ! surface pressure
!
!      Output
integer :: LBOT                      ! bottom layer number
real(4) :: BLMULT                    ! bottom layer fractional multiplier


!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
integer :: LBOTX                     ! unrestricted bottom layer number
real(4) :: DELPX                     ! 5% of layer thickness in pressure


!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none

!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!
!      Determine LBOT from PSURF by comparing to PLEV
       if(DEBUG) print*, 'getbot:PSURF ',PSURF
       LBOTX=MAXLAY
 10    DELPX=0.05*( PLEV(LBOTX+1) - PLEV(LBOTX) )
       IF (PSURF .LT. PLEV(LBOTX)+DELPX) THEN
          LBOTX=LBOTX - 1
          GOTO 10
       ENDIF
!
       IF (LBOTX .GT. NLAY) THEN
          LBOT = NLAY
       ELSE
          LBOT = LBOTX
       ENDIF
       if (DEBUG) write(6,'(a,X,I6,X,F9.3,X,F9.3)')  &
        'getbot: LBOT, PLEV(LBOT), PSURF ', LBOT,PLEV(LBOT),PSURF
!
!      Calc bottom layer multiplier (fractional layer)
       BLMULT = (PSURF - PLEV(LBOT))/(PLEV(LBOT+1) - PLEV(LBOT))
!
       IF (BLMULT .GT. 1.3) THEN
          WRITE(IOINFO,1010) BLMULT, LBOTX, NLAY
 1010     FORMAT('WARNING! excessive BLMULT=',F9.3,'; optimal LBOT=', &
         I3,' but layers end at NLAY=',I3)
!!!          print*,'getbot: setting BLMULT=1.2'
!!!          BLMULT=1.2
       ENDIF
!
       RETURN
       END
