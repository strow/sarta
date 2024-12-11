!=======================================================================
!
!    University of Maryland Baltimore Country (UMBC)
!
!    AIRS
!
!    QIKEXP
!
!F90====================================================================


!ROUTINE NAME:
!    QIKEXP (real function)


!ABSTRACT:
!    Quick approximate calculation of e^x.


!CALL PROTOCOL
!    QIKEXP( XVAL )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL      XVAL    exponent                    none


!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL fun  QIKEXP  exp(XVAL)                   none


!INPUT/OUTPUT PARAMETERS:
!    none


!RETURN VALUES:
!    none


!PARENT(S):
!    CALT1
!    CALT2
!    CALT3
!    CALT4
!    CALT5
!    CALT6
!    CALT7


!ROUTINES CALLED:
!    none


!FILES ACCESSED:
!    none


!COMMON BLOCKS
!    none


!DESCRIPTION:
!    March 1998 version of the 100 layer AIRS Fast Transmittance
!    Code by L.Strow/S.Hannon.
!
!    ===================================================================
!    Quick exponential calculation of e^x. The function looks at x and
!    if it is small, it does the exponential calculation by using just
!    the first few terms of the series expansion:
!         exp(x) = sum i=0 to inf of { x^n/n! }.
!    ===================================================================


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    None, but keep in mind it is only quicker than EXP if "x" is small.


!ROUTINE HISTORY:
!    Date        Programmer     Comments
!    ----------- -------------- ----------------------------------------
!     3 Feb 1997 Scott Hannon   Created as a stand-alone function
!    18 Jul 1997 Scott Hannon   Changed from 3 to 4 xval regions; same
!                               speed but more accurate


!END====================================================================

!      =================================================================
       REAL FUNCTION QIKEXP( XVAL )
!      =================================================================
!
!      QuIcK EXPonential
!
!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------
       IMPLICIT NONE


!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
!      none


!-----------------------------------------------------------------------
!      EXTERNAL FUNCTIONS
!-----------------------------------------------------------------------
!      none


!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
       REAL XVAL


!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
       REAL AXVAL


!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none


!***********************************************************************
!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!***********************************************************************
!
!      Absolute value of XVAL
       AXVAL=ABS(XVAL)
!
       IF (AXVAL .LT. 4.0E-03) THEN
!         Use the first two series terms only:
          QIKEXP=1.0E+0 + XVAL
!
       ELSEIF (AXVAL .LT. 3.6E-02) THEN
!         Use the first three series terms only:
          QIKEXP=1.0E+0 + XVAL + XVAL*XVAL*5.0E-1
!
       ELSEIF (AXVAL .LT. 1.2E-01) THEN
!         Use the first four series terms only:
          QIKEXP=1.0E+0 + ( XVAL*( 6.0E+0 + (XVAL*(3.0E+0 + XVAL)) ) &
            /6.0E+0 )
!
       ELSE
          QIKEXP=EXP(XVAL)
       ENDIF
!
       RETURN
       END
