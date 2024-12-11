!=======================================================================
 
! Code converted using TO_F90_LOOP by Alan Miller
! Date: 2023-04-04  Time: 16:44:56
 
!=======================================================================

!    University of Maryland Baltimore Country (UMBC)

!    AIRS

!    SPECCAL

!F77====================================================================


!ROUTINE NAME:
!    SPECCAL


!ABSTRACT:
!    Read spectral calibration adjustment for prof.freqcal


!CALL PROTOCOL
!    SPECCAL ( IOUN, DFCAL)


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INTEGER   IOUN    I/O unit number             none


!INPUT/OUTPUT PARAMETERS:
! none


!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL arr  DFCAL   delta for prof.freqcal      um


!RETURN VALUES:
!    none


!PARENT(S):
!    USEFAST


!ROUTINES CALLED:
!    none


!FILES ACCESSED:
!    incFTC.f : include file of parameter statements accessed during
!       compilation only.
!    unit IOUN : input text file of tuning adjustments.


!COMMON BLOCKS
!    none


!DESCRIPTION:
!    August 2009 version of SARTA v1.08 code by L.Strow/S.Hannon.

!    The routine reads a text file of spectral calibration adjustments
!    to be applied to the nominal prof.freqcal

!    The spectral calibration file must consist of MXCHAN lines of data
!    sorted (in ascending order) by channel ID and containing the
!    following 3 columns:
!       1    2     3
!       ID RJUNK DFCAL
!    where
!       ID = integer, channel ID number
!       RJUNK = real, value is ignored, eg perhaps channel freq
!       DFCAL = real, delta for prof.freqcal
!    Comment lines may be included anywhere in the tuning multiplier
!    by using a "!" or "%" as the first character on the line.



!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    none


!ROUTINE HISTORY:
! Date        Programmer     Comments
! ----------- -------------- -------------------------------------------
! 03 Aug 2009 Scott Hannon   Created
! 06 Aug 2009 Scott Hannon   Remove arguments NCHAN and INDCHN; return
!                               data for all channels

!END====================================================================

!      =================================================================

SUBROUTINE SPECCAL( IOUN, DFCAL )
!      =================================================================


!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------

INTEGER, INTENT(OUT)                     :: IOUN
REAL, INTENT(OUT)                        :: DFCAL(MXCHAN)
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
!      INPUT


!      OUTPUT



!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
CHARACTER (LEN=80) :: CLINE
REAL :: RJUNK
REAL :: RJUNK2
INTEGER :: I
INTEGER :: ICHAN
INTEGER :: IERR


!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none


!***********************************************************************
!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!***********************************************************************


!      ---------------------------------
!      Open the spec cal adjustment file
!      ---------------------------------
OPEN(UNIT=IOUN,FILE=FNSCAL,FORM='FORMATTED',STATUS='OLD', IOSTAT=IERR)
IF (IERR /= 0) THEN
  WRITE(6,1020) IERR, FNSCAL
  1020     FORMAT('Error ',I5,' opening file:',/,A80)
  STOP
END IF


!      Initialize the channel counter
I=0

!      -------------
!      Read the file
!      -------------
!      Read a line of text from the file
10    READ(IOUN,9000,END=910) CLINE
9000  FORMAT(A80)

!      Determine if the text line is data or a comment
IF (CLINE(1:1) /= '!' .AND. CLINE(1:1) /= '%') THEN
  
!         It's data, so increment the channel counter
  I=I+1
  
!         Read the data from the text line
  READ(CLINE,*)  ICHAN, RJUNK, RJUNK2
  
!         Check that ICHAN agrees with I
  IF (ICHAN /= I) THEN
    WRITE(6,1040) I, MXCHAN
    1040        FORMAT('Error reading spec cal file:',/,  &
        'Expected channel ID ',I4,' but file has ID ',I4)
    STOP
  END IF
  DFCAL(ICHAN)=RJUNK2
  
END IF

GO TO 10
910   CLOSE(IOUN)

IF (I /= MXCHAN) THEN
  WRITE(6,1050) I, MXCHAN
  1050     FORMAT('Error reading spec cal file:',/,  &
      'Read data for ',I5,' channels, but expected ',I5)
END IF


RETURN
END SUBROUTINE SPECCAL
