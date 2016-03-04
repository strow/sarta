c version for sarta
C=======================================================================
C=======================================================================
C
C              University of Maryland Baltimore County [UMBC]
C
C              AIRS
C
C              OPNRTP version with trace gases
C
!F77====================================================================


!ROUTINE NAME: OPNRTP


!ABSTRACT:
C    Open and check input RTP file.


!CALL PROTOCOL:
C    OPNRTP(FIN, LRHOT, PTYPE, NCHAN, FCHAN, LSTCHN, INDCHN,
C    IH2O, IO3, ICO, ICH4, ICO2, ISO2, IHNO3, IN2O,
C    IOPCI, HEAD, HATT, PATT, LCO2PM)



!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    CHAR*80   FIN     input RTP file name         none
C    LOGICAL   LRHOT   force refl therm rho?       none


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER   PTYPE   profile type                none
C    INTEGER   NCHAN   number of channels          none
C    INTEGER   FCHAN   channel frequencies         cm^-1
C    INTEGER   LSTCHN  list of channel numbers     none (1-2378)
C    INTEGER   INDCHN  indices of channels         none
C    INTEGER   IH2O    index of H2O in gamnt       none
C    INTEGER   IO3     index of O3 in gamnt        none
C    INTEGER   ICO     index of CO in gamnt        none
C    INTEGER   ICH4    index of CH4 in gamnt       none
C    INTEGER   ICO2    index of CO2 in gamnt       none
C    INTEGER   ISO2    index of SO2 in gamnt       none
C    INTEGER   IHNO3   index of HNO3 in gamnt      none
C    INTEGER   IN2O    index of N2O in gamnt       none
C    INTEGER   IOPCI   input RTP file I/O unit     none
C    INTEGER   IOPCO   output RTP file I/O unit    none
C    STRUCT    HEAD    RTP header structure        various
C    STRUCT    HATT    RTP header attributes       none
C    STRUCT    PATT    RTP profile attributes      none
C    LOGICAL   LCO2PM  CO2 profile in ppmv?        none


!INPUT/OUTPUT PARAMETERS: none


!RETURN VALUES: none


!PARENT(S): sarta_rtp


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    IOPCI : input RTP file I/O unit ("profile channel")


!COMMON BLOCKS: none


!DESCRIPTION:
C    Opens the input RTP file and reads the header info.
C    Checks the header info.


!ALGORITHM REFERENCES: see DESCRIPTION


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date     Programmer        Comments
C------------ ----------------- ----------------------------------------
C 13 Feb 2001 Scott Hannon      Created
C 23 Feb 2001 Scott Hannon      Added PROF.gunit and GUCIN check
C 28 Feb 2001 Scott Hannon      Add IOUN and read in chan freq file
C 14 Mar 2001 Scott Hannon      Add HEAD, HATT, and PATT to call
C                               paramters.   Removed open of output
C                               RTP; now done outside this routine.
C 13 Sep 2001 Scott Hannon      Added AIRSLAY to ptype check
C 21 Nov 2001 Scott Hannon      Remove CSARTA; change comment string
C                               to use VSARTA, VSCOEF, & VCLOUD; add
C                               CJUNK2, CJUNK3, & COMMNT; CJUNK
C                               decreased from 80 to 40;
C 20 Sep 2002 Scott Hannon      If exists, overwrite old "sarta" hattr
C 05 Aug 2003 Scott Hannon      Correct FIN to CHAR*80 (not 70)
C 06 Feb 2004 Scott Hannon      Add LRHOT & PTYPE to arguments and add
C                                  associated code.
C 18 May 2005 Scott Hannon      Add HNO3 based on from SO2 code
C 23 Jun 2005 Scott Hannon      "trace" version for CO2,SO2,HNO3,N2O
C 23 Jan 2008 Scott Hannon      Add LCO2PM to allow CO2 profile in ppmv
C 24 Oct 2008 Scott Hannon      Minor update for rtpV201
C 12 May 2009 Scott Hannon      Change VCLOUD to VTUNNG in "sarta" HATT


!END====================================================================


C      =================================================================
       SUBROUTINE OPNRTP(FIN, LRHOT, PTYPE, NCHAN, FCHAN, LSTCHN,
     $    INDCHN, IH2O, IO3, ICO, ICH4, ICO2, ISO2, IHNO3, IN2O,
     $    IOPCI, HEAD, HATT, PATT, LCO2PM)
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
C      From "util.f"
C      function LENNB = length of string excluding trailing blanks


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      Input
       CHARACTER*80 FIN        ! input RTP filename
       LOGICAL  LRHOT          ! force refl therm rho? {for COMMNT}
C
C      Output
       INTEGER  PTYPE          ! profile type
       INTEGER  NCHAN          ! number of channels
       REAL     FCHAN(MXCHAN)  ! channel freqs
       INTEGER LSTCHN(MXCHAN)  ! channel ID numbers
       INTEGER INDCHN(MXCHAN)  ! indices of channels
       INTEGER   IH2O          ! index of H2O in gamnt
       INTEGER    IO3          ! index of O3 in gamnt
       INTEGER    ICO          ! index of CO in gamnt
       INTEGER   ICH4          ! index of CH4 in gamnt
       INTEGER   ICO2          ! index of CO2 in gamnt
       INTEGER   ISO2          ! index of SO2 in gamnt
       INTEGER  IHNO3          ! index of HNO3 in gamnt
       INTEGER   IN2O          ! index of N2O in gamnt
       INTEGER  IOPCI  ! I/O unit ("profile channel") for input file
C
C      Structures (see "rtpdefs.f")
       RECORD /RTPHEAD/ HEAD            ! header data
       RECORD /RTPATTR/ HATT(MAXNATTR)  ! header attributes
       RECORD /RTPATTR/ PATT(MAXNATTR)  ! profile attributes

       LOGICAL LCO2PM          ! CO2 profile in ppmv?

C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER      I
       INTEGER     IC
c       INTEGER     IG
       INTEGER  NGASI          ! number of gases in input file
       INTEGER GLISTI( MXGAS)  ! list of gas IDs in input file
       INTEGER      J
       INTEGER      K
       INTEGER  LENNB        ! for function LENNB
       INTEGER  MEMIS        ! max number of emis pts
       INTEGER  NHATT        ! counter for # of header attributes
c       INTEGER  NPATT        ! counter for # of profile attributes
       INTEGER rtpopen       ! function rtpopen
       INTEGER STATUS        ! status of RTP file open
       CHARACTER*1 MODE      ! mode for rtpopen: "c"=create, "r"=read
       CHARACTER*1 CRHOT     ! LRHOT converted to character T or F
c       CHARACTER*14 CUNITS   ! string for gamnt units
       CHARACTER*40 CJUNK    ! junk/work string
       CHARACTER*40 CJUNK2   ! another junk/work string
       CHARACTER*40 CJUNK3   ! yet another junk/work string
       CHARACTER*256 COMMNT  ! comment string
C
C      for N2BITS and BITS2N
       INTEGER*4 NUMBER
       LOGICAL LFLAGS(32)
C
       LOGICAL LNEED         ! needed gas?

C-----------------------------------------------------------------------
C      SAVE STATEMENTS
C-----------------------------------------------------------------------
C      none


C***********************************************************************
C***********************************************************************
C      EXECUTABLE CODE begins below
C***********************************************************************
C***********************************************************************
C
C      -------------------
C      Open RTP input file
C      -------------------
       MODE='r'
       STATUS=rtpopen(FIN, MODE, HEAD, HATT, PATT, IOPCI)

ccc
c       print *, 'read open status = ', STATUS
ccc

C      -------------------------
C      Quick checks of input RTP
C      -------------------------
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
C      Note: if no RHO data will use (1-emis)/pi
ccc
c Removed 26 April 2001 by Scott Hannon since mlev may be less than MAXLAY+1
c       IF (HEAD.mlevs .NE. MAXLAY+1) THEN
c          WRITE(IOERR,1005) MAXLAY
c 1005     FORMAT('Error! input RTP is not the ',I3,' AIRS layers')
c          STOP
c       ENDIF
ccc
       NCHAN=HEAD.nchan
       IF (NCHAN .LT. 1) THEN
          WRITE(IOERR,1007)
 1007     FORMAT('Error! input RTP has no channel info')
          STOP
       ENDIF
C
       IF (MEMIS .GT. MXEMIS) THEN
          WRITE(IOERR,1008) MEMIS, MXEMIS
 1008     FORMAT('ERROR! input RTP HEAD.memis=',I4,
     $    ' exceeds MXEMIS=',I4)
       ENDIF
C
       NUMBER=HEAD.pfields
       CALL N2BITS(NUMBER, LFLAGS)
       IF (.NOT. LFLAGS(1)) THEN  ! PROFBIT is bit1
          WRITE(IOERR,1010)
 1010     FORMAT('ERROR! input RTP file has no profile data!')
          STOP
       ENDIF
C

C      -----------
C      Check gases
C      -----------
       LCO2PM=.FALSE.
       IH2O =-1
       ICO2 =-1
       IO3  =-1
       IN2O =-1
       ICO  =-1
       ICH4 =-1
       ISO2 =-1
       IHNO3=-1
C
C      Loop over gases
       NGASI=HEAD.ngas
       DO I=1,NGASI
          GLISTI(I)=HEAD.glist(I)
          LNEED=.FALSE.
C
C         Determine indices of needed gases
C         Note: will abort if a needed gas is not present
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
C
C         Determine indices of trace gases
C         Note: will use reference amount if a trace gas is not present
C         Exception: CO2 will use CO2PPM
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
          IF (GLISTI(I) .EQ. 12) THEN
             IHNO3=I
             LNEED=.TRUE.
          ENDIF
C
C         Check gas units
          IF (LNEED) THEN
             IF (HEAD.gunit(I) .NE. GUCIN) THEN
                IF (I .EQ. ICO2 .AND. HEAD.gunit(I) .EQ. 10) THEN
                   LCO2PM=.TRUE.
      print *, 'CO2 profile in ppmv'
                ELSE
                   WRITE(IOERR,1020) GUCIN, I, HEAD.gunit(I)
 1020              FORMAT('ERROR! Wrong gas units code number. ',
     $             'Need ',I3,' but HEAD.gunit(',I2,')=',I3)
                   STOP
                ENDIF
             ENDIF
          ENDIF
C
       ENDDO
C
C      Abort if a needed gas is not present
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
C
C      Print a warning if a trace gas is not present
       IF (ICO2  .LT. 1) THEN
          WRITE(IOERR,1035) 2, 'CO2 '
       ENDIF
       IF (IN2O  .LT. 1) THEN
          WRITE(IOERR,1035) 4, 'N2O '
       ENDIF
       IF (ISO2 .LT. 1) THEN
          WRITE(IOERR,1035) 9, 'SO2 '
       ENDIF
       IF (IHNO3 .LT. 1) THEN
          WRITE(IOERR,1035) 12, 'HNO3'
       ENDIF
 1035  FORMAT('Warning! input files does not contain gas ',I2,' = ',A4)
C

C      -------------------
C      Create channel list
C      -------------------
C      Initialize channel index list
       DO I=1,MXCHAN
          INDCHN(I)=0
       ENDDO
       K=0  ! initialize counter
       DO I=1,NCHAN
          J=HEAD.ichan(I)  ! channel ID
          FCHAN(I)=HEAD.vchan(I)  ! channel freq (or junk if unfilled)
C
          IF ((J .LT. 1) .OR. (J .GT. MXCHAN)) THEN
             WRITE(IOERR,1042) MXCHAN, J
 1042        FORMAT('Error! Channel number is out of range.',/,
     $       'Range is 1 to ',I4,', but input RTP has ',I7)
             STOP
          ENDIF       
C
          IF (INDCHN(J) .EQ. 0) THEN
C            Not a repeat
             K=K + 1  ! increment counter (should be same as I)
             LSTCHN(K)=J
             INDCHN(J)=K
C
          ELSE
             WRITE(IOERR,1044) J
 1044        FORMAT('ERROR! input RTP has repeat of ',
     $       'channel ',I4)
             STOP
          ENDIF
       ENDDO
C
C      --------------
C      Update pfields
C      --------------
       LFLAGS(2)=.TRUE.  ! IRCALCBIT is bit2
       CALL BITS2N(NUMBER, LFLAGS)
       HEAD.pfields=NUMBER
C

C      -----------------------------------
C      Update header attributes for output
C      -----------------------------------
C      Add sarta comment to header attributes
C      Count the number of header attributes
       I=1
       IC=-1
       DO WHILE (ICHAR(HATT(I).fname) .NE. 0 .AND. I .LE. MAXNATTR)
C         Look for a previous sarta comment
          IF (HATT(I).aname(1:5) .EQ. 'sarta') IC=I
          I=I + 1
       ENDDO
       IF (IC .LT. 1) THEN
C         Create a new hatt entry for sarta
          IC=I
          NHATT=IC
       ELSE
C         No new hatt entry need for sarta (will overwrite old comment)
          NHATT=I - 1
       ENDIF
C
       CJUNK=VSARTA
       CJUNK2=VSCOEF
       CJUNK3=VTUNNG
       I=LENNB(CJUNK)
       J=LENNB(CJUNK2)
       K=LENNB(CJUNK3)
C
       IF (LRHOT) THEN
          CRHOT='T'
       ELSE
          CRHOT='F'
       ENDIF
       COMMNT='SARTA src=' // CJUNK(1:I) // '; coef=' // CJUNK2(1:J)
     $ // '; tuning=' // CJUNK3(1:K) // '; LRHOT=' // CRHOT // CHAR(0)
       J=LENNB(COMMNT)
       HATT(IC).fname='header'  // CHAR(0)
       HATT(IC).aname='sarta' // CHAR(0)
       HATT(IC).atext=COMMNT(1:J)
C
C      Add a char(0) to end of attributes if less than maxnattr
       IF (NHATT .LT. MAXNATTR) THEN
          HATT(NHATT + 1).fname=CHAR(0)
       ENDIF
C

ccc do not bother to check profile attributes cccccccccccccccccccccccccc
cC      --------------------------------------
cC      Check input rtp gas_i units attributes
cC      --------------------------------------
c       I=1
c       DO WHILE (ICHAR(PATT(I).fname) .NE. 0 .AND. I .LE. MAXNATTR)
cC
c          IF ( PATT(I).fname(1:4) .EQ. 'gas_' .AND.
c     $         PATT(I).aname(1:5) .EQ. 'units') THEN
cC
cC            Pull out gas ID (integer) from fname (string)
c             IC=ICHAR( PATT(I).fname(6:6) )
c             IF (IC .GE. ICHAR('0') .AND. IC .LE. ICHAR('9')) THEN
c                READ( PATT(I).fname(5:6), *) IG
c             ELSE
c                READ( PATT(I).fname(5:5), *) IG
c             ENDIF
cC
cC            Expecting "kilomoles/cm^2"
cccc
ccC            Expecting "molecules/cm^2"
cccc
c             CUNITS=PATT(I).atext(1:14)
c             CALL UPCASE(CUNITS)
c
cC            Expecting "kilomoles/cm^2"
c             IF (CUNITS .NE. 'KILOMOLES/CM^2') THEN
cccc
ccC            Expecting "molecules/cm^2"
cc             IF (CUNITS .NE. 'MOLECULES/CM^2') THEN
cccc
c                IF (IG .EQ. 1) THEN
c                   WRITE(IOERR,1050) 1, CUNITS
c 1050              FORMAT('ERROR! units for gas ',I2,
c     $               ' are ',A14,' instead of kilomoles/cm^2')
cccc
cc     $               ' are ',A14,' instead of molecules/cm^2')
cccc
c                   STOP
c                ELSEIF (IG .EQ. 3) THEN
c                   WRITE(IOERR,1050) 3, CUNITS
c                   STOP
c                ELSEIF (IG .EQ. 5) THEN
c                   WRITE(IOERR,1050) 5, CUNITS
c                   STOP
c                ELSEIF (IG .EQ. 6) THEN
c                   WRITE(IOERR,1050) 6, CUNITS
c                   STOP
c                ENDIF
cC
c             ENDIF
cC
c          ENDIF
c          I=I + 1  ! increment attribute counter
c       ENDDO
c       NPATT=I - 1  ! count of profile attributes in input file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C

C
       RETURN
       END
