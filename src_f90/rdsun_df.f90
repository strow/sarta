!=======================================================================
 
! Code converted using TO_F90_LOOP by Alan Miller
! Date: 2023-04-04  Time: 16:44:56
 
!=======================================================================

!    University of Maryland Baltimore Country (UMBC)

!    AIRS

!    RDSUN

!F77====================================================================


!ROUTINE NAME:
!    RDSUN


!ABSTRACT:
!    Read in the AIRS solar radiance data


!CALL PROTOCOL
!    RDSUN ( AORB, IOUN, INDCHN, HSUN )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    CHAR*1    AORB    specify A or B              none
!    INT arr   INDCHN  indices of channels         none
!    INTEGER   IOUN    I/O unit number             none


!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL arr  HSUN    solar radiance              W/(m2.str.cm-1)


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
!    unit IOUN : input file, ASCII text file. The file is opened,
!       read, and closed.


!COMMON BLOCKS
!    none


!DESCRIPTION:
!    May 2008 version of the 100 layer AIRS Fast Transmittance
!    Code by L.Strow/S.Hannon.

!    Reads in a text file with solar radiance data for each AIRS
!    channel.  This is the solar rad direct from the sun at the top
!    of Earth's atmosphere.


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    none


!ROUTINE HISTORY:
! Date        Programmer     Comments
! ----------- -------------- ----------------------------------------
! 12 Sep 1997 Scott Hannon   Created
! 12 Feb 2001 Scott Hannon   hardcoded filename instead of prompt
! 09 May 2008 Scott Hannon   Add AORB

!END====================================================================

!      =================================================================

SUBROUTINE RDSUN ( AORB, IOUN, INDCHN, HSUN )
!      =================================================================


!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------

CHARACTER (LEN=1), INTENT(IN OUT)        :: AORB
INTEGER, INTENT(OUT)                     :: IOUN
INTEGER, INTENT(OUT)                     :: INDCHN(MXCHAN)
REAL, INTENT(OUT)                        :: HSUN(MXCHAN)
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
!      Input




!      Output



!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
CHARACTER (LEN=80) :: FNSUN
CHARACTER (LEN=80) :: CLINE
INTEGER :: I
INTEGER :: IERR
INTEGER :: ICHAN
REAL :: FRQCHN
REAL :: SUNCHN


!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none


!***********************************************************************
!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!***********************************************************************

!      Solar filename
IF (AORB == 'A') THEN
  FNSUN=FASUN
ELSE
  FNSUN=FBSUN
END IF

!      ----------------------------
!      Open the solar radiance file
!      ----------------------------
OPEN(UNIT=IOUN,FILE=FNSUN,FORM='FORMATTED',STATUS='OLD', IOSTAT=IERR)
IF (IERR /= 0) THEN
  WRITE(6,1020) IERR, FNSUN
  1020     FORMAT('Error ',I5,' opening file:',/,A80)
  STOP
END IF

!      Initialize the channel counter
I=0

!      -----------------------
!      Read the solar rad file
!      -----------------------
!      Read a line of text from the file
10    READ(IOUN,9000,END=910) CLINE
9000  FORMAT(A80)

!      Determine if the text line is data or a comment
IF (CLINE(1:1) /= '!') THEN
  
!         It's data, so increment the channel counter
  I=I+1
  
!         Read the data from the text line
  READ(CLINE,*)  ICHAN, FRQCHN, SUNCHN
  
!         Check to be sure the channel value is OK
  IF ((ICHAN < 1) .OR. (ICHAN > MXCHAN)) THEN
    WRITE(6,1040) MXCHAN, ICHAN
    1040        FORMAT('Error! Channel number is out of range.',/,  &
        'Range is 1 to ',I4,', but channel list has ',I7,'.')
    STOP
  END IF
  
!         Keep the data if the current channel is on the list
  IF (INDCHN(ICHAN) /= 0) THEN
    HSUN( INDCHN(ICHAN) )=SUNCHN
  END IF
  
END IF

!      Read the next line
IF (I < MXCHAN) GO TO 10
!      Note: this routine expects data for every channel

910   CLOSE(IOUN)

RETURN
END SUBROUTINE RDSUN
