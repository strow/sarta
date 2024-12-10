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
C    Calculate a "fake" layer-to-space optical depth.
    

!CALL PROTOCOL:
C    FAKETZ ( NFAKE, INDFAK, LBOT, TAUZ, SEC, SECFAK, TAUZFK )


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER   INDFAK  array indices for fake      none
C    INTEGER   NFAKE   number of fake points       none
C    INTEGER   LBOT    layer index for fake OD     none
C    REAL arr  SEC     angle secant for TAUZ       none
C    REAL arr  SECFAK  angle secant for TAUZFK     none
C    REAL arr  TAUZ    layer-to-space op depth     none


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    REAL arr  TAUZFK  fake layer-to-space OD      none


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
C    A "fake" layer-to-space optical depth is calculated for some
C    arbitrary angle by scaling the optical depth by the ratio of
C    the angle secants.  The exact form of the calculation is:
C       TAUZFK = TAUZ * SECFAK/SEC
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
C    22 Dec 2006 Scott Hannon   Added LFAKE; change from trans to OD;
C                               TAUZ & TAUZFK from (1 x n) to (m x n)
C    08 Jan 2007 Scott Hannon   Added loop over layers; rename LFAKE
C                               to LBOT; make SECANG & SECSUN arrays

!END====================================================================

C      =================================================================
       SUBROUTINE FAKETZ ( NFAKE, INDFAK, LBOT, TAUZ, SEC, SECFAK,
     $    TAUZFK )
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
       INTEGER  LBOT
       REAL   TAUZ(MAXLAY,MXCHAN)
       REAL    SEC(MAXLAY)
       REAL SECFAK(MAXLAY)
       REAL TAUZFK(MAXLAY,MXCHAN)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER      I
       INTEGER  ICHAN
       INTEGER      L
       REAL RATSEC(MAXLAY)


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
       DO L=1,LBOT
          RATSEC(L)=SECFAK(L)/SEC(L)
       ENDDO
C
C      ---------------------------
C      Loop on channel (frequency)
C      ---------------------------
       DO I=1,NFAKE
C
          ICHAN=INDFAK(I)
          DO L=1,LBOT
C            Be careful to avoid log(0) ln(1E-8)=-18.4
             IF (TAUZ(L,ICHAN) .LT. 18.4) THEN
                TAUZFK(L,ICHAN)=RATSEC(L)*TAUZ(L,ICHAN)
             ELSE
                TAUZFK(L,ICHAN)=23.0
             ENDIF
          ENDDO
C
       ENDDO
C
       RETURN
       END
