C=======================================================================
C=======================================================================
C
C              University of Maryland Baltimore County [UMBC]
C
C              AIRS
C
C              WRTRTP
C
!F77====================================================================


!ROUTINE NAME: WRTRTP


!ABSTRACT:
C    Write a profile to a previously openned RTP file


!CALL PROTOCOL:
C    WRTRTP(IP, IOPCO, NCHAN, RAD, PROF)


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER   IP      profile count so far        none
C    INTEGER   IOPCO   input RTP file I/O number   none
C    INTEGER   NCHAN   # of channels               none
C    REAL arr  RAD     radiance                    W/m^2/cm^-1/sterad
C    STRUCT    PROF    RTP profile structure       (see attributes)


!OUTPUT PARAMETERS:
C    none


!INPUT/OUTPUT PARAMETERS: none


!RETURN VALUES: none


!PARENT(S): KLAYERS


!ROUTINES CALLED: none


!FILES ACCESSED:
C    Output RTP file with I/O number IOPCO
C    unit IOERR: error message


!COMMON BLOCKS: none


!DESCRIPTION:
C    Writes a single profile to a previously openned RTP file.


!ALGORITHM REFERENCES: see DESCRIPTION


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date     Programmer        Comments
C------------ ----------------- ----------------------------------------
C 14 Feb 2001 Scott Hannon      created based on version for klayers
C 23 Oct 2008 Scott Hannon      Minor update for rtpV201

!END====================================================================

C      =================================================================
       SUBROUTINE WRTRTP(IP, IOPCO, NCHAN, RAD, PROF)
C      =================================================================


C-----------------------------------------------------------------------
C      IMPLICIT NONE
C-----------------------------------------------------------------------
       IMPLICIT NONE


C-----------------------------------------------------------------------
C      INCLUDE FILES
C-----------------------------------------------------------------------
      include 'incFTC.f'
      include 'rtpdefs.f'


C-----------------------------------------------------------------------
C      EXTERNAL FUNCTIONS
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      Input parameters:
       INTEGER     IP
       INTEGER  IOPCO
       INTEGER  NCHAN
       REAL    RAD(MXCHAN)
C
C      Profile data structure
       RECORD /RTPPROF/ PROF


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER I
       INTEGER ISTAT
       INTEGER rtpwrite


C-----------------------------------------------------------------------
C      SAVE STATEMENTS
C-----------------------------------------------------------------------
C      none


C***********************************************************************
C***********************************************************************
C      EXECUTABLE CODE begins below
C***********************************************************************
C***********************************************************************

C      -------------------------
C      Load up the new PROF data
C      -------------------------
C      Loop over the channels
       DO I=1,NCHAN
C         Convert from Watts/m^2/cm^-1 to milliWatts/m^2/cm^-1
          PROF.rcalc(I)=RAD(I)*1000.0
       ENDDO
C
C      -------------------------
C      Write the current profile
C      -------------------------
       ISTAT=rtpwrite(IOPCO, PROF)
C
       IF (ISTAT .EQ. -1) THEN
          WRITE(IOERR,1010) IP
 1010     FORMAT('ERROR! unable to write PROF data for prof ',I5)
       ENDIF
C
       RETURN
       END
