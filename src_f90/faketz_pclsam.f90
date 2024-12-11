!=======================================================================
 
! Code converted using TO_F90_LOOP by Alan Miller
! Date: 2023-04-04  Time: 16:44:55
 
!=======================================================================

!    University of Maryland Baltimore County [UMBC]

!    AIRS

!    FAKETZ

!F77====================================================================


!ROUTINE NAME:
!    FAKETZ


!ABSTRACT:
!    Calculate a "fake" layer-to-space optical depth.


!CALL PROTOCOL:
!    FAKETZ ( NFAKE, INDFAK, LBOT, TAUZ, SEC, SECFAK, TAUZFK )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INTEGER   INDFAK  array indices for fake      none
!    INTEGER   NFAKE   number of fake points       none
!    INTEGER   LBOT    layer index for fake OD     none
!    REAL arr  SEC     angle secant for TAUZ       none
!    REAL arr  SECFAK  angle secant for TAUZFK     none
!    REAL arr  TAUZ    layer-to-space op depth     none


!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL arr  TAUZFK  fake layer-to-space OD      none


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

!    A "fake" layer-to-space optical depth is calculated for some
!    arbitrary angle by scaling the optical depth by the ratio of
!    the angle secants.  The exact form of the calculation is:
!       TAUZFK = TAUZ * SECFAK/SEC
!    This is a crude approximation of the correct value.


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    This is a crude approximation of the correct value.


!ROUTINE HISTORY:
!    Date        Programmer     Comments
!    ----------- -------------- ----------------------------------------
!    Aug 27 1997 Scott Hannon   Created
!    Aug 27 1998 Scott Hannon   Fix bug for case when TAUZ=0
!    22 Dec 2006 Scott Hannon   Added LFAKE; change from trans to OD;
!                               TAUZ & TAUZFK from (1 x n) to (m x n)
!    08 Jan 2007 Scott Hannon   Added loop over layers; rename LFAKE
!                               to LBOT; make SECANG & SECSUN arrays

!END====================================================================

!      =================================================================

SUBROUTINE FAKETZ ( NFAKE, INDFAK, LBOT, TAUZ, SEC, SECFAK, TAUZFK )
!      =================================================================

!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------

INTEGER, INTENT(IN)                      :: NFAKE
INTEGER, INTENT(IN)                      :: INDFAK(MXCHAN)
INTEGER, INTENT(IN)                      :: LBOT
REAL, INTENT(IN)                         :: TAUZ(MAXLAY,MXCHAN)
REAL, INTENT(IN)                         :: SEC(MAXLAY)
REAL, INTENT(IN)                         :: SECFAK(MAXLAY)
REAL, INTENT(OUT)                        :: TAUZFK(MAXLAY,MXCHAN)
IMPLICIT NONE


!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
INCLUDE 'incFTC.f'


!-----------------------------------------------------------------------
!      EXTERNAL FUNCTIONS
!-----------------------------------------------------------------------
!      none


!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------









!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
INTEGER :: I
INTEGER :: ICHAN
INTEGER :: L
REAL :: RATSEC(MAXLAY)


!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none


!***********************************************************************
!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!***********************************************************************

!      Calc ratio of secants
DO L=1,LBOT
  RATSEC(L)=SECFAK(L)/SEC(L)
  ENDDO
    
!      ---------------------------
!      Loop on channel (frequency)
!      ---------------------------
    DO I=1,NFAKE
      
      ICHAN=INDFAK(I)
      DO L=1,LBOT
!            Be careful to avoid log(0) ln(1E-8)=-18.4
        IF (TAUZ(L,ICHAN) < 18.4) THEN
          TAUZFK(L,ICHAN)=RATSEC(L)*TAUZ(L,ICHAN)
        ELSE
          TAUZFK(L,ICHAN)=23.0
        END IF
        ENDDO
          
          ENDDO
            
            RETURN
          END SUBROUTINE FAKETZ
