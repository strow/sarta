!=======================================================================
!
!              University of Maryland Baltimore County [UMBC]
!
!              AIRS
!
!              WRTRTP
!
!F90====================================================================


!ROUTINE NAME: WRTRTP


!ABSTRACT:
!    Write a profile to a previously openned RTP file


!CALL PROTOCOL:
!    WRTRTP(IP, IOPCO, NCHAN, RAD, PROF)


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INTEGER   IP      profile count so far        none
!    INTEGER   IOPCO   input RTP file I/O number   none
!    INTEGER   NCHAN   # of channels               none
!    REAL arr  RAD     radiance                    W/m^2/cm^-1/sterad
!    STRUCT    PROF    RTP profile structure       (see attributes)


!OUTPUT PARAMETERS:
!    none


!INPUT/OUTPUT PARAMETERS: none


!RETURN VALUES: none


!PARENT(S): KLAYERS


!ROUTINES CALLED: none


!FILES ACCESSED:
!    Output RTP file with I/O number IOPCO
!    unit IOERR: error message


!COMMON BLOCKS: none


!DESCRIPTION:
!    Writes a single profile to a previously openned RTP file.


!ALGORITHM REFERENCES: see DESCRIPTION


!KNOWN BUGS AND LIMITATIONS:
!    none


!ROUTINE HISTORY:
!    Date     Programmer        Comments
!------------ ----------------- ----------------------------------------
! 14 Feb 2001 Scott Hannon      created based on version for klayers
! 23 Oct 2008 Scott Hannon      Minor update for rtpV201

!END====================================================================

!      =================================================================
       SUBROUTINE WRTRTP(IP, IOPCO, NCHAN, RAD, PROF)
!      =================================================================

!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
use incFTC

!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------
       IMPLICIT NONE

include 'rtpdefs.f90'

!-----------------------------------------------------------------------
!      EXTERNAL FUNCTIONS
!-----------------------------------------------------------------------
!      none

!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
!      Input parameters:
integer :: IP,  IOPCO, NCHAN
real(4), dimension(MXCHAN) :: RAD
!      Profile data structure
       RECORD /RTPPROF/ PROF

!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
integer :: I, ISTAT, rtpwrite

!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none

!***********************************************************************
!***********************************************************************
!      EXECUTABLE CODE begins below
!***********************************************************************
!***********************************************************************

!      -------------------------
!      Load up the new PROF data
!      -------------------------
!      Loop over the channels
       DO I=1,NCHAN
!         Convert from Watts/m^2/cm^-1 to milliWatts/m^2/cm^-1
          PROF%rcalc(I)=RAD(I)*1000.0
!          PROF.rcalc(I)=0.98765
       ENDDO
!
!      -------------------------
!      Write the current profile
!      -------------------------
       ISTAT=rtpwrite(IOPCO, PROF)
!
       IF (ISTAT .EQ. -1) THEN
          WRITE(IOERR,1010) IP
 1010     FORMAT('ERROR! unable to write PROF data for prof ',I5)
       ENDIF
!
       RETURN
       END
