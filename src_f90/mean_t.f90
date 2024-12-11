!=======================================================================
!
!    University of Maryland Baltimore Country (UMBC)
!
!    AIRS
!
!    MEAN_T
!
!F90====================================================================


!ROUTINE NAME:
!    MEAN_T


!ABSTRACT:
!    Convert PGE L2 pseudo-level T's to layer mean T's.


!CALL PROTOCOL
!    MEAN_T( LBOT, PLEV, PSURF, TPSEUD, TLAY )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INTEGER   LBOT    number of layers            none
!    REAL arr  PLEV    pressure levels             millibar
!    REAL      PSURF   surface pressure            millibar
!    REAL      TPSEUD  pseudo level temperatures   Kelvin


!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL arr  TLAY    layer mean temperature      Kelvin


!INPUT/OUTPUT PARAMETERS:
!    none


!RETURN VALUES:
!    none


!PARENT(S):
!    SARTA


!ROUTINES CALLED:
!    none


!FILES ACCESSED:
!    none


!COMMON BLOCKS
!    none


!DESCRIPTION:
!    Converts the AIRS PGE Level2 pseudo-level temperature profile
!    to the layer mean temperature profile as required by the RTA.
!    The layer mean T's are defined in the PGE as the average of
!    the two adjacent level pseudo-level T's.


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    SARTA assumes an input layer profile is full layers, but
!    this routine has a built in adjustment for the bottom
!    factional layer T.  Thus SARTA needs to be aware of this
!    and skip the usual adjustment for the bottom layer.


!ROUTINE HISTORY:
!    Date        Programmer     Comments
!    ----------- -------------- ----------------------------------------
!    05 Feb 2004 Scott Hannon   Created; based on translation formula
!                                  from the PGE's "meantemp.F"
!
!
!END =====================================================================
!
!      =================================================================
       SUBROUTINE MEAN_T(LBOT, PLEV, PSURF, TPSEUD, TLAY)
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
       INTEGER   LBOT        ! # of layers for this profile
       REAL  PSURF           ! surface pressure
       REAL   PLEV(MAXLAY+1) ! pressure levels
       REAL TPSEUD(MAXLAY)   ! pseudo level temperature at PLEV(L+1)
!
!      Output
       REAL   TLAY(MAXLAY)   ! layer mean temperature


!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
       INTEGER L            ! layer index
       REAL TSURFA          ! Air temperature at surface


!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none


!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************

!      Do top layer (special case)
       TLAY(1) = TPSEUD(1)
!
!      Loop down over the layers
       DO L=2,LBOT-1
          TLAY(L) = 0.5*( TPSEUD(L-1) + TPSEUD(L) )
       ENDDO
!
!      Interpolate to get air temperature at the surface
       TSURFA = TPSEUD(LBOT-1) + ( TPSEUD(LBOT) - TPSEUD(LBOT-1) )* &
         ( PSURF - PLEV(LBOT) )/( PLEV( LBOT+1) - PLEV(LBOT) )
!
!      Do bottom layer (special case)
       TLAY(LBOT) = 0.5*( TPSEUD(LBOT-1) + TSURFA )
!
       RETURN
       END
