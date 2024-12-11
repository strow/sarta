!=======================================================================
!
!    University of Maryland Baltimore County [UMBC]
!
!    AIRS
!
!    FAKETZ
!
!F90====================================================================


!ROUTINE NAME:
!    FAKETZ


!ABSTRACT:
!    Calculate a "fake" layer-to-space transmittance.
    

!CALL PROTOCOL:
!    FAKETZ ( NFAKE, INDFAK, TAUZ, SEC, SECFAK, TAUZFK )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INTEGER   INDFAK  array indices for fake      none
!    INTEGER   NFAKE   number of fake points       none
!    REAL      SEC     angle secant for TAUZ       none
!    REAL      SECFAK  angle secant for TAUZFK     none
!    REAL arr  TAUZ    layer-to-space trans        none


!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL arr  TAUZFK  fake layer-to-space trans   none


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
!
!    A "fake" layer-to-space transmittance is calculated for some
!    arbitrary angle by scaling the optical depth by the ratio of
!    the angle secants.  The exact form of the calculation is:
!       TAUZFK = EXP( LN(TAUZ) * SECFAK/SEC )
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


!END====================================================================

!      =================================================================
       SUBROUTINE FAKETZ ( NFAKE, INDFAK, TAUZ, SEC, SECFAK, TAUZFK )
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
       INTEGER  NFAKE
       INTEGER INDFAK(MXCHAN)
       REAL   TAUZ(MXCHAN)
       REAL    SEC
       REAL SECFAK
       REAL TAUZFK(MXCHAN)

!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
       INTEGER      I
       INTEGER  ICHAN
       REAL RATSEC

!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none

!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!
!      Calc ratio of secants
       RATSEC=SECFAK/SEC
!
!      ---------------------------
!      Loop on channel (frequency)
!      ---------------------------
       DO I=1,NFAKE
!
          ICHAN=INDFAK(I)
!         Be careful to avoid log(0)
          IF (TAUZ(ICHAN) .GT. 1E-8) THEN
             TAUZFK(ICHAN)=EXP( RATSEC*LOG( TAUZ(ICHAN) ) )
          ELSE
             TAUZFK(ICHAN)=1E-10
          ENDIF
!
       ENDDO
!
       RETURN
       END
