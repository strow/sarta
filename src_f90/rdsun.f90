!=======================================================================
!=======================================================================
!
!    University of Maryland Baltimore Country (UMBC)
!
!    AIRS
!
!    RDSUN
!
!F77====================================================================


!ROUTINE NAME:
!    RDSUN


!ABSTRACT:
!    Read in the AIRS solar radiance data


!CALL PROTOCOL
!    RDSUN ( IOUN, INDCHN, HSUN )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
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
!    August 2000 version of the 100 layer AIRS Fast Transmittance
!    Code by L.Strow/S.Hannon.
!
!    Reads in a text file with solar radiance data for each AIRS
!    channel.  This is the solar rad direct from the sun at the top
!    of Earth's atmosphere.


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    none


!ROUTINE HISTORY:
!    Date        Programmer     Comments
!    ----------- -------------- ----------------------------------------
! version2 with hardcoded include filename instead of prompt
!    12 Sep 1997 Scott Hannon   Created
!    12 Feb 2001 Scott Hannon   hardcoded filename instead of prompt


!END====================================================================

!      =================================================================
       SUBROUTINE RDSUN ( IPOPN, INDCHN, HSUN )
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
integer :: IPOPN
integer, dimension(MXCHAN) :: INDCHN
!      Output
real(4), dimension(MXCHAN) :: HSUN
! LOCAL VARIABLES
character(len=80) :: CLINE
integer :: I, IERR, ICHAN
real(4) :: FRQCHN, SUNCHN

!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none


!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!
!      ----------------------------
!      Open the solar radiance file
!      ----------------------------
       OPEN(UNIT=IOUN,FILE=FNSUN,FORM='FORMATTED',STATUS='OLD', &
         IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNSUN
 1020     FORMAT('Error ',I5,' opening file:',/,A90)
          STOP
       ENDIF
!
!      Initialize the channel counter
       I=0

!      -----------------------
!      Read the solar rad file
!      -----------------------
!      Read a line of text from the file
 10    READ(IOUN,9000,END=910) CLINE
 9000  FORMAT(A80)
!
!      Determine if the text line is data or a comment
       IF (CLINE(1:1) .NE. '!') THEN
!
!         It's data, so increment the channel counter
          I=I+1
!
!         Read the data from the text line
          READ(CLINE,*)  ICHAN, FRQCHN, SUNCHN
!
!         Check to be sure the channel value is OK
          IF ((ICHAN .LT. 1) .OR. (ICHAN .GT. MXCHAN)) THEN
             WRITE(6,1040) MXCHAN, ICHAN
 1040        FORMAT('Error! Channel number is out of range.',/, &
            'Range is 1 to ',I4,', but channel list has ',I7,'.')
             STOP
          ENDIF
!
!         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             HSUN( INDCHN(ICHAN) )=SUNCHN
          ENDIF
!
       ENDIF
!
!      Read the next line
       IF (I .LT. MXCHAN) GOTO 10
!      Note: this routine expects data for every channel
!
 910   CLOSE(IOUN)
!
       RETURN
       END
