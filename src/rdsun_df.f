C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore Country (UMBC)
C
C    AIRS
C
C    RDSUN
C
!F77====================================================================


!ROUTINE NAME:
C    RDSUN


!ABSTRACT:
C    Read in the AIRS solar radiance data


!CALL PROTOCOL
C    RDSUN ( AORB, IOUN, INDCHN, HSUN )


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    CHAR*1    AORB    specify A or B              none
C    INT arr   INDCHN  indices of channels         none
C    INTEGER   IOUN    I/O unit number             none


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    REAL arr  HSUN    solar radiance              W/(m2.str.cm-1)


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
C    unit IOUN : input file, ASCII text file. The file is opened,
C       read, and closed.


!COMMON BLOCKS
C    none


!DESCRIPTION:
C    May 2008 version of the 100 layer AIRS Fast Transmittance
C    Code by L.Strow/S.Hannon.
C
C    Reads in a text file with solar radiance data for each AIRS
C    channel.  This is the solar rad direct from the sun at the top
C    of Earth's atmosphere.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C Date        Programmer     Comments
C ----------- -------------- ----------------------------------------
C 12 Sep 1997 Scott Hannon   Created
C 12 Feb 2001 Scott Hannon   hardcoded filename instead of prompt
C 09 May 2008 Scott Hannon   Add AORB

!END====================================================================

C      =================================================================
       SUBROUTINE RDSUN ( AORB, IOUN, INDCHN, HSUN )
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
       CHARACTER*1   AORB
       INTEGER   IOUN
       INTEGER INDCHN(MXCHAN)
C
C      Output
       REAL   HSUN(MXCHAN)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       CHARACTER*80  FNSUN
       CHARACTER*80  CLINE
       INTEGER      I
       INTEGER   IERR
       INTEGER  ICHAN
       REAL FRQCHN
       REAL SUNCHN


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
C      Solar filename
       IF (AORB .EQ. 'A') THEN
          FNSUN=FASUN
       ELSE
          FNSUN=FBSUN
       ENDIF

C      ----------------------------
C      Open the solar radiance file
C      ----------------------------
       OPEN(UNIT=IOUN,FILE=FNSUN,FORM='FORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNSUN
 1020     FORMAT('Error ',I5,' openning file:',/,A80)
          STOP
       ENDIF
C
C      Initialize the channel counter
       I=0

C      -----------------------
C      Read the solar rad file
C      -----------------------
C      Read a line of text from the file
 10    READ(IOUN,9000,END=910) CLINE
 9000  FORMAT(A80)
C
C      Determine if the text line is data or a comment
       IF (CLINE(1:1) .NE. '!') THEN
C
C         It's data, so increment the channel counter
          I=I+1
C
C         Read the data from the text line
          READ(CLINE,*)  ICHAN, FRQCHN, SUNCHN
C
C         Check to be sure the channel value is OK
          IF ((ICHAN .LT. 1) .OR. (ICHAN .GT. MXCHAN)) THEN
             WRITE(6,1040) MXCHAN, ICHAN
 1040        FORMAT('Error! Channel number is out of range.',/,
     $       'Range is 1 to ',I4,', but channel list has ',I7,'.')
             STOP
          ENDIF
C
C         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             HSUN( INDCHN(ICHAN) )=SUNCHN
          ENDIF
C
       ENDIF
C
C      Read the next line
       IF (I .LT. MXCHAN) GOTO 10
C      Note: this routine expects data for every channel
C
 910   CLOSE(IOUN)
C
       RETURN
       END
