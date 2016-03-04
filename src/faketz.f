C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore County [UMBC]
C
C    AIRS
C
C    FAKETZ
C
!F77====================================================================


!ROUTINE NAME:
C    FAKETZ


!ABSTRACT:
C    Calculate a "fake" layer-to-space transmittance.
    

!CALL PROTOCOL:
C    FAKETZ ( NFAKE, INDFAK, TAUZ, SEC, SECFAK, TAUZFK )


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER   INDFAK  array indices for fake      none
C    INTEGER   NFAKE   number of fake points       none
C    REAL      SEC     angle secant for TAUZ       none
C    REAL      SECFAK  angle secant for TAUZFK     none
C    REAL arr  TAUZ    layer-to-space trans        none


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    REAL arr  TAUZFK  fake layer-to-space trans   none


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
C
C    A "fake" layer-to-space transmittance is calculated for some
C    arbitrary angle by scaling the optical depth by the ratio of
C    the angle secants.  The exact form of the calculation is:
C       TAUZFK = EXP( LN(TAUZ) * SECFAK/SEC )
C    This is a crude approximation of the correct value.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    This is a crude approximation of the correct value.


!ROUTINE HISTORY:
C    Date        Programmer     Comments
C    ----------- -------------- ----------------------------------------
C    Aug 27 1997 Scott Hannon   Created
C    Aug 27 1998 Scott Hannon   Fix bug for case when TAUZ=0


!END====================================================================

C      =================================================================
       SUBROUTINE FAKETZ ( NFAKE, INDFAK, TAUZ, SEC, SECFAK, TAUZFK )
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
       INTEGER  NFAKE
       INTEGER INDFAK(MXCHAN)
       REAL   TAUZ(MXCHAN)
       REAL    SEC
       REAL SECFAK
       REAL TAUZFK(MXCHAN)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER      I
       INTEGER  ICHAN
       REAL RATSEC


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
C      Calc ratio of secants
       RATSEC=SECFAK/SEC
C
C      ---------------------------
C      Loop on channel (frequency)
C      ---------------------------
       DO I=1,NFAKE
C
          ICHAN=INDFAK(I)
C         Be careful to avoid log(0)
          IF (TAUZ(ICHAN) .GT. 1E-8) THEN
             TAUZFK(ICHAN)=EXP( RATSEC*LOG( TAUZ(ICHAN) ) )
          ELSE
             TAUZFK(ICHAN)=1E-10
          ENDIF
C
       ENDDO
C
       RETURN
       END
