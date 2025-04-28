! version for sarta
!=======================================================================
!
!              University of Maryland Baltimore County [UMBC]
!
!              AIRS
!
!              OPNRTP version with trace gase NH3
!
!F90====================================================================

!ROUTINE NAME: OPNRTP

!ABSTRACT:
!    Open and check input RTP file.

!CALL PROTOCOL:
!    OPNRTP(FIN, LRHOT, PTYPE, NCHAN, FCHAN, LSTCHN, INDCHN,
!    IH2O, IO3, ICO, ICH4, ICO2, ISO2, IHNO3, IN2O, INH3,
!    IOPCI, HEAD, HATT, PATT, LCO2PM)

!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    CHAR*80   FIN     input RTP file name         none
!    LOGICAL   LRHOT   force refl therm rho?       none

!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INTEGER   PTYPE   profile type                none
!    INTEGER   NCHAN   number of channels          none
!    INTEGER   FCHAN   channel frequencies         cm^-1
!    INTEGER   LSTCHN  list of channel numbers     none (1-2378)
!    INTEGER   INDCHN  indices of channels         none
!    INTEGER   IH2O    index of H2O in gamnt       none
!    INTEGER   IO3     index of O3 in gamnt        none
!    INTEGER   ICO     index of CO in gamnt        none
!    INTEGER   ICH4    index of CH4 in gamnt       none
!    INTEGER   ICO2    index of CO2 in gamnt       none
!    INTEGER   ISO2    index of SO2 in gamnt       none
!    INTEGER   IHNO3   index of HNO3 in gamnt      none
!    INTEGER   IN2O    index of N2O in gamnt       none
!    INTEGER   INH3    index of NH3 in gamnt       none
!    INTEGER   IOPCI   input RTP file I/O unit     none
!    INTEGER   IOPCO   output RTP file I/O unit    none
!    STRUCT    HEAD    RTP header structure        various
!    STRUCT    HATT    RTP header attributes       none
!    STRUCT    PATT    RTP profile attributes      none
!    LOGICAL   LCO2PM  CO2 profile in ppmv?        none

!INPUT/OUTPUT PARAMETERS: none

!RETURN VALUES: none

!PARENT(S): sarta_rtp

!ROUTINES CALLED:
!    none

!FILES ACCESSED:
!    IOPCI : input RTP file I/O unit ("profile channel")

!COMMON BLOCKS: none

!DESCRIPTION:
!    Opens the input RTP file and reads the header info.
!    Checks the header info.

!ALGORITHM REFERENCES: see DESCRIPTION

!KNOWN BUGS AND LIMITATIONS:
!    none

!ROUTINE HISTORY:
!    Date     Programmer        Comments
!------------ ----------------- ----------------------------------------
! 13 Feb 2001 Scott Hannon      Created
! 23 Feb 2001 Scott Hannon      Added PROF.gunit and GUCIN check
! 28 Feb 2001 Scott Hannon      Add IOUN and read in chan freq file
! 14 Mar 2001 Scott Hannon      Add HEAD, HATT, and PATT to call
!                               paramters.   Removed open of output
!                               RTP; now done outside this routine.
! 13 Sep 2001 Scott Hannon      Added AIRSLAY to ptype check
! 21 Nov 2001 Scott Hannon      Remove CSARTA; change comment string
!                               to use VSARTA, VSCOEF, & VCLOUD; add
!                               CJUNK2, CJUNK3, & COMMNT; CJUNK
!                               decreased from 80 to 40;
! 20 Sep 2002 Scott Hannon      If exists, overwrite old "sarta" hattr
! 05 Aug 2003 Scott Hannon      Correct FIN to CHAR*80 (not 70)
! 06 Feb 2004 Scott Hannon      Add LRHOT & PTYPE to arguments and add
!                                  associated code.
! 18 May 2005 Scott Hannon      Add HNO3 based on from SO2 code
! 23 Jun 2005 Scott Hannon      "trace" version for CO2,SO2,HNO3,N2O
! 23 Jan 2008 Scott Hannon      Add LCO2PM to allow CO2 profile in ppmv
! 24 Oct 2008 Scott Hannon      Minor update for rtpV201
! 12 May 2009 Scott Hannon      Change VCLOUD to VTUNNG in "sarta" HATT
! 10 May 2018 C Hepplewhite     Add NH3

!END====================================================================


!      =================================================================
       SUBROUTINE OPNRTP(FIN, LRHOT, PTYPE, NCHAN, FCHAN, LSTCHN, &
         INDCHN, IH2O, IO3, ICO, ICH4, ICO2, ISO2, IHNO3, IN2O, INH3, &
         IOPCI, HEAD, HATT, PATT, LCO2PM)
!      =================================================================

!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
use incFTC

!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------
IMPLICIT NONE

include "rtpdefs.f90"

!-----------------------------------------------------------------------
!      EXTERNAL FUNCTIONS
!-----------------------------------------------------------------------
!      From "util.f"
!      function LENNB = length of string excluding trailing blanks

!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
!      Input
       CHARACTER*80 FIN        ! input RTP filename
       LOGICAL  LRHOT          ! force refl therm rho? {for COMMNT}
!
!      Output
integer :: PTYPE, NCHAN                       ! profile type, # channels
integer :: IH2O, IO3, ICO, ICH4, ICO2, ISO2, IHNO3, IN2O, INH3 ! gamt index
real(4), dimension(MXCHAN) :: FCHAN           ! channel freqs
integer, dimension(MXCHAN) :: LSTCHN, INDCHN  ! channel ID numbers,indexes
integer :: IOPCI                              ! I/O unit ("profile channel") for input file
!
!      Structures (see "rtpdefs.f")
       RECORD /RTPHEAD/ HEAD            ! header data
       RECORD /RTPATTR/ HATT(MAXNATTR)  ! header attributes
       RECORD /RTPATTR/ PATT(MAXNATTR)  ! profile attributes

logical :: LCO2PM          ! CO2 profile in ppmv?

!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
integer :: I, J, K, IC         ! local loop counters
!       INTEGER     IG
       INTEGER  NGASI          ! number of gases in input file
       INTEGER GLISTI( MXGAS)  ! list of gas IDs in input file
       INTEGER  LENNB        ! for function LENNB
       INTEGER  MEMIS        ! max number of emis pts
       INTEGER  NHATT        ! counter for # of header attributes
!       INTEGER  NPATT        ! counter for # of profile attributes
       INTEGER rtpopen       ! function rtpopen
       INTEGER STATUS        ! status of RTP file open
       CHARACTER*1 MODE      ! mode for rtpopen: "c"=create, "r"=read
       CHARACTER*1 CRHOT     ! LRHOT converted to character T or F
!       CHARACTER*14 CUNITS   ! string for gamnt units
       CHARACTER*40 CJUNK    ! junk/work string
       CHARACTER*40 CJUNK2   ! another junk/work string
       CHARACTER*40 CJUNK3   ! yet another junk/work string
       CHARACTER*256 COMMNT  ! comment string
!       CHARACTER*60 VTUNNG   ! optical depth tuning version
!
!      for N2BITS and BITS2N
       INTEGER*4 NUMBER
       LOGICAL LFLAGS(32)
!
       LOGICAL LNEED         ! needed gas?

!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none


!***********************************************************************
!***********************************************************************
!      EXECUTABLE CODE begins below
!***********************************************************************
!***********************************************************************
!
!      -------------------
!      Open RTP input file
!      -------------------
       MODE='r'
       STATUS=rtpopen(FIN, MODE, HEAD, HATT, PATT, IOPCI)

!cc
       if(DEBUG) print *, 'opnrtp: read open status = ', STATUS
!cc

!      -------------------------
!      Quick checks of input RTP
!      -------------------------
       MEMIS=HEAD.memis
       PTYPE=HEAD.ptype
       IF (PTYPE .NE. LAYPRO .AND. PTYPE .NE. AIRSLAY) THEN
          WRITE(IOERR,1003)
 1003     FORMAT('Error! input RTP ptype must be LAYPRO or AIRSLAY')
          STOP
       ENDIF
       IF (MEMIS .LT. 1) THEN
          WRITE(IOERR,1004)
 1004     FORMAT('Error! input RTP has no emissivity info')
          STOP
       ENDIF
!      Note: if no RHO data will use (1-emis)/pi
!cc
! Removed 26 April 2001 by Scott Hannon since mlev may be less than MAXLAY+1
!       IF (HEAD.mlevs .NE. MAXLAY+1) THEN
!          WRITE(IOERR,1005) MAXLAY
! 1005     FORMAT('Error! input RTP is not the ',I3,' AIRS layers')
!          STOP
!       ENDIF
!cc
       NCHAN=HEAD.nchan
!!       WRITE(6,*) "opnrtp: NCHAN = ",NCHAN
       IF (NCHAN .LT. 1) THEN
          WRITE(IOERR,1007)
 1007     FORMAT('Error! input RTP has no channel info')
          STOP
       ENDIF
!
       IF (MEMIS .GT. MXEMIS) THEN
          WRITE(IOERR,1008) MEMIS, MXEMIS
 1008     FORMAT('ERROR! input RTP HEAD.memis=',I4,' exceeds MXEMIS=',I4)
       ENDIF
!
       NUMBER=HEAD.pfields
       CALL N2BITS(NUMBER, LFLAGS)
       IF (.NOT. LFLAGS(1)) THEN  ! PROFBIT is bit1
          WRITE(IOERR,1010)
 1010     FORMAT('ERROR! input RTP file has no profile data!')
          STOP
       ENDIF
!

!      -----------
!      Check gases
!      -----------
       LCO2PM=.FALSE.
       IH2O =-1
       ICO2 =-1
       IO3  =-1
       IN2O =-1
       ICO  =-1
       ICH4 =-1
       ISO2 =-1
       INH3 =-1
       IHNO3=-1
!
!      Loop over gases
       NGASI=HEAD.ngas
       DO I=1,NGASI
          GLISTI(I)=HEAD.glist(I)
          LNEED=.FALSE.
!
!         Determine indices of needed gases
!         Note: will abort if a needed gas is not present
          IF (GLISTI(I) .EQ.  1) THEN
             IH2O=I
             LNEED=.TRUE.
          ENDIF
          IF (GLISTI(I) .EQ.  3) THEN
             IO3=I
             LNEED=.TRUE.
          ENDIF
          IF (GLISTI(I) .EQ.  5) THEN
             ICO=I
             LNEED=.TRUE.
          ENDIF
          IF (GLISTI(I) .EQ.  6) THEN
             ICH4=I
             LNEED=.TRUE.
          ENDIF
!
!         Determine indices of trace gases
!         Note: will use reference amount if a trace gas is not present
!         Exception: CO2 will use CO2PPM
          IF (GLISTI(I) .EQ.  2) THEN
             ICO2=I
             LNEED=.TRUE.
          ENDIF
          IF (GLISTI(I) .EQ.  4) THEN
             IN2O=I
             LNEED=.TRUE.
          ENDIF
          IF (GLISTI(I) .EQ.  9) THEN
             ISO2=I
             LNEED=.TRUE.
          ENDIF
          IF (GLISTI(I) .EQ. 11) THEN
             INH3=I
             LNEED=.TRUE.
          ENDIF
          IF (GLISTI(I) .EQ. 12) THEN
             IHNO3=I
             LNEED=.TRUE.
          ENDIF
!
!         Check gas units
          IF (LNEED) THEN
             IF (HEAD.gunit(I) .NE. GUCIN) THEN
                IF (I .EQ. ICO2 .AND. HEAD.gunit(I) .EQ. 10) THEN
                   LCO2PM=.TRUE.
!      print *, 'CO2 profile in ppmv'
                ELSE
                   WRITE(IOERR,1020) GUCIN, I, HEAD.gunit(I)
 1020              FORMAT('ERROR! Wrong gas units code number. ',&
                  'Need ',I3,' but HEAD.gunit(',I2,')=',I3)
                   STOP
                ENDIF
             ENDIF
          ENDIF
!
       ENDDO
!
!      Abort if a needed gas is not present
       IF (IH2O .LT. 1) THEN
          WRITE(IOERR,1030) 1, 'H2O '
          STOP
       ELSEIF (IO3  .LT. 1) THEN
          WRITE(IOERR,1030) 3, 'O3  '
          STOP
       ELSEIF (ICO  .LT. 1) THEN
          WRITE(IOERR,1030) 5, 'CO  '
          STOP
       ELSEIF (ICH4 .LT. 1) THEN
          WRITE(IOERR,1030) 6, 'CH4 '
          STOP
       ENDIF
 1030  FORMAT('Error! input files does not contain gas ',I2,' = ',A4)
!
!      Print a warning if a trace gas is not present
       IF (ICO2  .LT. 1) THEN
          WRITE(IOERR,1035) 2, 'CO2 '
       ENDIF
       IF (IN2O  .LT. 1) THEN
          WRITE(IOERR,1035) 4, 'N2O '
       ENDIF
       IF (ISO2 .LT. 1) THEN
          WRITE(IOERR,1035) 9, 'SO2 '
       ENDIF
       IF (INH3 .LT. 1) THEN
          WRITE(IOERR,1035) 11, 'NH3 '
       ENDIF
       IF (IHNO3 .LT. 1) THEN
          WRITE(IOERR,1035) 12, 'HNO3'
       ENDIF
 1035  FORMAT('Warning! input files does not contain gas ',I2,' = ',A4)
!

!      -------------------
!      Create channel list
!      -------------------
!      Initialize channel index list
       DO I=1,MXCHAN
          INDCHN(I)=0
       ENDDO
!       DO I=830,845
!         write(6,*) 'opnrtp: HEAD.ichan(I): ',HEAD.ichan(I)
!       enddo
       K=0  ! initialize counter
       DO I=1,NCHAN   ! NCHAN
          J=HEAD.ichan(I)  ! channel ID
          FCHAN(I)=HEAD.vchan(I)  ! channel freq (or junk if unfilled)
!
          IF ((J .LT. 1) .OR. (J .GT. MXCHAN)) THEN
             WRITE(IOERR,1042) MXCHAN, J
 1042        FORMAT('Error! Channel number is out of range.',/,&
            'Range is 1 to ',I4,', but input RTP has ',I7)
             STOP
          ENDIF       
!
          IF (INDCHN(J) .EQ. 0) THEN
!            Not a repeat
             K=K + 1  ! increment counter (should be same as I)
             LSTCHN(K)=J
             INDCHN(J)=K
!
!          ELSE
!             WRITE(IOERR,1044) J
! 1044        FORMAT('ERROR! input RTP has repeat of channel ',I4)
!             STOP
          ENDIF
!          write(6,*) 'opnrtp: I,J,K, INDCHN(J),FCHAN(I): ',I,J,K,INDCHN(J), FCHAN(I)
       ENDDO
!!       write(6,*) 'opnrtp: I,J,K, INDCHN(J),LSTCHN(K): ' &
!!                    ,I,J-1,K-1,INDCHN(J-1), LSTCHN(K-1)
!
!      --------------
!      Update pfields
!      --------------
       LFLAGS(2)=.TRUE.  ! IRCALCBIT is bit2
       CALL BITS2N(NUMBER, LFLAGS)
       HEAD.pfields=NUMBER
!

!      -----------------------------------
!      Update header attributes for output
!      -----------------------------------
!      Add sarta comment to header attributes
!      Count the number of header attributes
       I=1
       IC=-1
       DO WHILE (ICHAR(HATT(I)%fname(1:1)) .NE. 0 .AND. I .LE. MAXNATTR)
!         Look for a previous sarta comment
          IF (HATT(I).aname(1:5) .EQ. 'sarta') IC=I
          I=I + 1
       ENDDO
       IF (IC .LT. 1) THEN
!         Create a new hatt entry for sarta
          IC=I
          NHATT=IC
       ELSE
!         No new hatt entry need for sarta (will overwrite old comment)
          NHATT=I - 1
       ENDIF
!
       CJUNK=VSARTA
       CJUNK2=VSCOEF
       CJUNK3=VTUNNG
       I=LENNB(CJUNK)
       J=LENNB(CJUNK2)
       K=LENNB(CJUNK3)
!
       IF (LRHOT) THEN
          CRHOT='T'
       ELSE
          CRHOT='F'
       ENDIF
       COMMNT='SARTA src=' // CJUNK(1:I) // '; coef=' // CJUNK2(1:J) &
       // '; tuning=' // CJUNK3(1:K) // '; LRHOT=' // CRHOT // CHAR(0)
       J=LENNB(COMMNT)
       HATT(IC).fname='header'  // CHAR(0)
       HATT(IC).aname='sarta' // CHAR(0)
       HATT(IC).atext=COMMNT(1:J)
!
!      Add a char(0) to end of attributes if less than maxnattr
       IF (NHATT .LT. MAXNATTR) THEN
          HATT(NHATT + 1).fname=CHAR(0)
       ENDIF
!

!cc do not bother to check profile attributes cccccccccccccccccccccccccc
!C      --------------------------------------
!C      Check input rtp gas_i units attributes
!C      --------------------------------------
!       I=1
!       DO WHILE (ICHAR(PATT(I).fname) .NE. 0 .AND. I .LE. MAXNATTR)
!C
!          IF ( PATT(I).fname(1:4) .EQ. 'gas_' .AND.
!     $         PATT(I).aname(1:5) .EQ. 'units') THEN
!C
!C            Pull out gas ID (integer) from fname (string)
!             IC=ICHAR( PATT(I).fname(6:6) )
!             IF (IC .GE. ICHAR('0') .AND. IC .LE. ICHAR('9')) THEN
!                READ( PATT(I).fname(5:6), *) IG
!             ELSE
!                READ( PATT(I).fname(5:5), *) IG
!             ENDIF
!C
!C            Expecting "kilomoles/cm^2"
!ccc
!cC            Expecting "molecules/cm^2"
!ccc
!             CUNITS=PATT(I).atext(1:14)
!             CALL UPCASE(CUNITS)
!
!C            Expecting "kilomoles/cm^2"
!             IF (CUNITS .NE. 'KILOMOLES/CM^2') THEN
!ccc
!cC            Expecting "molecules/cm^2"
!c             IF (CUNITS .NE. 'MOLECULES/CM^2') THEN
!ccc
!                IF (IG .EQ. 1) THEN
!                   WRITE(IOERR,1050) 1, CUNITS
! 1050              FORMAT('ERROR! units for gas ',I2,
!     $               ' are ',A14,' instead of kilomoles/cm^2')
!ccc
!c     $               ' are ',A14,' instead of molecules/cm^2')
!ccc
!                   STOP
!                ELSEIF (IG .EQ. 3) THEN
!                   WRITE(IOERR,1050) 3, CUNITS
!                   STOP
!                ELSEIF (IG .EQ. 5) THEN
!                   WRITE(IOERR,1050) 5, CUNITS
!                   STOP
!                ELSEIF (IG .EQ. 6) THEN
!                   WRITE(IOERR,1050) 6, CUNITS
!                   STOP
!                ENDIF
!C
!             ENDIF
!C
!          ENDIF
!          I=I + 1  ! increment attribute counter
!       ENDDO
!       NPATT=I - 1  ! count of profile attributes in input file
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!

!
       if(DEBUG) print*,'opnrtp: returning from opnrtp'
       RETURN
       END
