C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore Country (UMBC)
C
C    AIRS
C
C    QIKEXP
C
!F77====================================================================


!ROUTINE NAME:
C    QIKEXP (real function)


!ABSTRACT:
C    Quick approximate calculation of e^x.


!CALL PROTOCOL
C    QIKEXP( XVAL )


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    REAL      XVAL    exponent                    none


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    REAL fun  QIKEXP  exp(XVAL)                   none


!INPUT/OUTPUT PARAMETERS:
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    CALT1
C    CALT2
C    CALT3
C    CALT4
C    CALT5
C    CALT6
C    CALT7


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    none


!COMMON BLOCKS
C    none


!DESCRIPTION:
C    March 1998 version of the 100 layer AIRS Fast Transmittance
C    Code by L.Strow/S.Hannon.
C
C    ===================================================================
C    Quick exponential calculation of e^x. The function looks at x and
C    if it is small, it does the exponential calculation by using just
C    the first few terms of the series expansion:
C         exp(x) = sum i=0 to inf of { x^n/n! }.
C    ===================================================================


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    None, but keep in mind it is only quicker than EXP if "x" is small.


!ROUTINE HISTORY:
C    Date        Programmer     Comments
C    ----------- -------------- ----------------------------------------
C     3 Feb 1997 Scott Hannon   Created as a stand-alone function
C    18 Jul 1997 Scott Hannon   Changed from 3 to 4 xval regions; same
C                               speed but more accurate


!END====================================================================

C      =================================================================
       REAL FUNCTION QIKEXP( XVAL )
C      =================================================================
C
C      QuIcK EXPonential
C
C-----------------------------------------------------------------------
C      IMPLICIT NONE
C-----------------------------------------------------------------------
       IMPLICIT NONE


C-----------------------------------------------------------------------
C      INCLUDE FILES
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      EXTERNAL FUNCTIONS
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
       REAL XVAL


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       REAL AXVAL


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
C      Absolute value of XVAL
       AXVAL=ABS(XVAL)
C
       IF (AXVAL .LT. 4.0E-03) THEN
C         Use the first two series terms only:
          QIKEXP=1.0E+0 + XVAL
C
       ELSEIF (AXVAL .LT. 3.6E-02) THEN
C         Use the first three series terms only:
          QIKEXP=1.0E+0 + XVAL + XVAL*XVAL*5.0E-1
C
       ELSEIF (AXVAL .LT. 1.2E-01) THEN
C         Use the first four series terms only:
          QIKEXP=1.0E+0 + ( XVAL*( 6.0E+0 + (XVAL*(3.0E+0 + XVAL)) )
     $       /6.0E+0 )
C
       ELSE
          QIKEXP=EXP(XVAL)
       ENDIF
C
       RETURN
       END
