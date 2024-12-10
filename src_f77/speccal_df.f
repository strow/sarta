C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore Country (UMBC)
C
C    AIRS
C
C    SPECCAL
C
!F77====================================================================


!ROUTINE NAME:
C    SPECCAL


!ABSTRACT:
C    Read spectral calibration adjustment for prof.freqcal


!CALL PROTOCOL
C    SPECCAL ( IOUN, DFCAL)


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER   IOUN    I/O unit number             none


!INPUT/OUTPUT PARAMETERS:
C none


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    REAL arr  DFCAL   delta for prof.freqcal      um


!RETURN VALUES:
C    none


!PARENT(S):
C    USEFAST


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    incFTC.f : include file of parameter statements accessed during
C       compilation only.
C    unit IOUN : input text file of tuning adjustments.


!COMMON BLOCKS
C    none


!DESCRIPTION:
C    August 2009 version of SARTA v1.08 code by L.Strow/S.Hannon.
C
C    The routine reads a text file of spectral calibration adjustments
C    to be applied to the nominal prof.freqcal
C
C    The spectral calibration file must consist of MXCHAN lines of data
C    sorted (in ascending order) by channel ID and containing the
C    following 3 columns:
C       1    2     3
C       ID RJUNK DFCAL
C    where
C       ID = integer, channel ID number
C       RJUNK = real, value is ignored, eg perhaps channel freq
C       DFCAL = real, delta for prof.freqcal
C    Comment lines may be included anywhere in the tuning multiplier
C    by using a "!" or "%" as the first character on the line.
C


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C Date        Programmer     Comments
C ----------- -------------- -------------------------------------------
C 03 Aug 2009 Scott Hannon   Created
C 06 Aug 2009 Scott Hannon   Remove arguments NCHAN and INDCHN; return
C                               data for all channels

!END====================================================================

C      =================================================================
       SUBROUTINE SPECCAL( IOUN, DFCAL )
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
C      INPUT
       INTEGER   IOUN
C
C      OUTPUT
       REAL  DFCAL(MXCHAN)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       CHARACTER*80  CLINE
       REAL  RJUNK
       REAL RJUNK2
       INTEGER      I
       INTEGER  ICHAN
       INTEGER   IERR


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

C      ---------------------------------
C      Open the spec cal adjustment file
C      ---------------------------------
       OPEN(UNIT=IOUN,FILE=FNSCAL,FORM='FORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNSCAL
 1020     FORMAT('Error ',I5,' opening file:',/,A80)
          STOP
       ENDIF
C

C      Initialize the channel counter
       I=0

C      -------------
C      Read the file
C      -------------
C      Read a line of text from the file
 10    READ(IOUN,9000,END=910) CLINE
 9000  FORMAT(A80)
C
C      Determine if the text line is data or a comment
       IF (CLINE(1:1) .NE. '!' .AND. CLINE(1:1) .NE. '%') THEN
C
C         It's data, so increment the channel counter
          I=I+1
C
C         Read the data from the text line
          READ(CLINE,*)  ICHAN, RJUNK, RJUNK2
C
C         Check that ICHAN agrees with I
          IF (ICHAN .NE. I) THEN
             WRITE(6,1040) I, MXCHAN
 1040        FORMAT('Error reading spec cal file:',/,
     $       'Expected channel ID ',I4,' but file has ID ',I4)
             STOP
          ENDIF
          DFCAL(ICHAN)=RJUNK2

       ENDIF
C
       GOTO 10
 910   CLOSE(IOUN)
C
       IF (I .NE. MXCHAN) THEN
          WRITE(6,1050) I, MXCHAN
 1050     FORMAT('Error reading spec cal file:',/,
     $    'Read data for ',I5,' channels, but expected ',I5)
       ENDIF

C
       RETURN
       END
