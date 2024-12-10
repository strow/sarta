C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore Country (UMBC)
C
C    AIRS
C
C    MEAN_T
C
!F77====================================================================


!ROUTINE NAME:
C    MEAN_T


!ABSTRACT:
C    Convert PGE L2 pseudo-level T's to layer mean T's.


!CALL PROTOCOL
C    MEAN_T( LBOT, PLEV, PSURF, TPSEUD, TLAY )


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER   LBOT    number of layers            none
C    REAL arr  PLEV    pressure levels             millibar
C    REAL      PSURF   surface pressure            millibar
C    REAL      TPSEUD  pseudo level temperatures   Kelvin


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    REAL arr  TLAY    layer mean temperature      Kelvin


!INPUT/OUTPUT PARAMETERS:
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    SARTA


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    none


!COMMON BLOCKS
C    none


!DESCRIPTION:
C    Converts the AIRS PGE Level2 pseudo-level temperature profile
C    to the layer mean temperature profile as required by the RTA.
C    The layer mean T's are defined in the PGE as the average of
C    the two adjacent level pseudo-level T's.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    SARTA assumes an input layer profile is full layers, but
C    this routine has a built in adjustment for the bottom
C    factional layer T.  Thus SARTA needs to be aware of this
C    and skip the usual adjustment for the bottom layer.


!ROUTINE HISTORY:
C    Date        Programmer     Comments
C    ----------- -------------- ----------------------------------------
C    05 Feb 2004 Scott Hannon   Created; based on translation formula
C                                  from the PGE's "meantemp.F"
C
C
!END =====================================================================
C
C      =================================================================
       SUBROUTINE MEAN_T(LBOT, PLEV, PSURF, TPSEUD, TLAY)
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
       INTEGER   LBOT        ! # of layers for this profile
       REAL  PSURF           ! surface pressure
       REAL   PLEV(MAXLAY+1) ! pressure levels
       REAL TPSEUD(MAXLAY)   ! pseudo level temperature at PLEV(L+1)
C
C      Output
       REAL   TLAY(MAXLAY)   ! layer mean temperature


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER L            ! layer index
       REAL TSURFA          ! Air temperature at surface


C-----------------------------------------------------------------------
C      SAVE STATEMENTS
C-----------------------------------------------------------------------
C      none


C***********************************************************************
C***********************************************************************
C                    EXECUTABLE CODE
C***********************************************************************
C***********************************************************************

C      Do top layer (special case)
       TLAY(1) = TPSEUD(1)
C
C      Loop down over the layers
       DO L=2,LBOT-1
          TLAY(L) = 0.5*( TPSEUD(L-1) + TPSEUD(L) )
       ENDDO
C
C      Interpolate to get air temperature at the surface
       TSURFA = TPSEUD(LBOT-1) + ( TPSEUD(LBOT) - TPSEUD(LBOT-1) )*
     $    ( PSURF - PLEV(LBOT) )/( PLEV( LBOT+1) - PLEV(LBOT) )
C
C      Do bottom layer (special case)
       TLAY(LBOT) = 0.5*( TPSEUD(LBOT-1) + TSURFA )
C
       RETURN
       END
