c version for sarta
C=======================================================================
C=======================================================================
C
C              University of Maryland Baltimore County [UMBC]
C
C              AIRS
C
C              RDRTP version with trace gases
C
!F77====================================================================


!ROUTINE NAME: RDRTP


!ABSTRACT:
C    Read a profile from a previously openned RTP file


!CALL PROTOCOL:
C    RDRTP( LWANT, IPROF, IOPCI,
C       IH2O, IO3, ICO, ICH4, ICO2, ISO2, IHNO3, IN2O,
C       PTYPE, RALT,LCO2PM,NLAY, NEMIS, LAT, LON, SATANG, SATZEN,
C       ZSAT, SUNANG, PSURF, TSURF, CO2PPM, FEMIS, EMIS, RHO,
C       TEMP, WAMNT, OAMNT, CAMNT, MAMNT, FAMNT, SAMNT, HAMNT, NAMNT,
C       ALT, PROF, ISTAT )


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    LOGICAL   LWANT   do you want this profile?   none
C    INTEGER   IPROF   profile number              none
C    INTEGER   IOPCI   input RTP file I/O number   none
C    INTEGER   IH2O    index of H2O in gamnt       none
C    INTEGER   IO3     index of O3 in gamnt        none
C    INTEGER   ICO     index of CO in gamnt        none
C    INTEGER   ICH4    index of CH4 in gamnt       none
C    INTEGER   ICO2    index of CO2 in gamnt       none
C    INTEGER   ISO2    index of SO2 in gamnt       none
C    INTEGER   IHNO3   index of HNO3 in gamnt      none
C    INTEGER   IN2O    index of N2O in gamnt       none
C    INTEGER   PTYPE   profile type code number    none
C    REAL arr  RALT    ref prof layer altitudes    meters
C    LOGICAL   LCO2PM  CO2 profile in ppmv?        none


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER   NLAY    number of used layers       none
C    INTEGER   NEMIS   number of used emis pts     none
C    REAL      LAT     latitude                    degrees
C    REAL      LON     longitude                   degrees
C    REAL      SATANG  satellite scan angle        degrees
C    REAL      SATZEN  satellite zenith angle      degrees
C    REAL      ZSAT    satellite altitude          kilometer
C    REAL      SUNANG  sun zenith angle            degrees
C    REAL      PSURF   surface pressure            millibars
C    REAL      TSURF   surface skin temperature    Kelvin
C    REAL      CO2PPM  mean trop-strat CO2 mix rat PPMV
C    REAL arr  FEMIS   emis freq points            cm^-1
C    REAL arr  EMIS    emis points                 none (0 to 1)
C    REAL arr  RHO     rho points                  none (0 to 1/pi)
C    REAL arr  TEMP    layer temperature           Kelvin
C    REAL arr  WAMNT   layer Water vapor amount    */cm^2
C    REAL arr  OAMNT   layer Ozone amount          */cm^2
C    REAL arr  CAMNT   layer CO amount             */cm^2
C    REAL arr  MAMNT   layer Methane amount        */cm^2
C    REAL arr  FAMNT   layer CO2 amount            */cm^2
C    REAL arr  SAMNT   layer SO2 amount            */cm^2
C    REAL arr  HAMNT   layer HNO3 amount           */cm^2
C    REAL arr  NAMNT   layer N2O amount            */cm^2
C    REAL arr  ALT     layer average altitude      meters
C    STRUCT    PROF    RTP profile structure       various
C    INTEGER   ISTAT   I/O status                  none
C note: units "*/cm^2" can be either kilomoles/cm^2 or molecules/cm^2


!INPUT/OUTPUT PARAMETERS: none


!RETURN VALUES: none


!PARENT(S): KLAYERS


!ROUTINES CALLED: none


!FILES ACCESSED:
C    Input RTP file withI/O number IOPCI
C    unit IOERR: error messages
C    unit IOINFO: info/warning messages


!COMMON BLOCKS: none


!DESCRIPTION:
C    Reads a single profile from a previously openned RTP file.
C    The routine expects to find the data specified in the header
C    of the input RTP file.


!ALGORITHM REFERENCES: see DESCRIPTION


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date     Programmer        Comments
C------------ ----------------- ----------------------------------------
C 14 Feb 2001 Scott Hannon      created based on klayers version
C 13 Sep 2001 Scott Hannon      Added checks of PSURF & TSURF
C 31 Oct 2002 Scott Hannon      Add output vars SATZEN and ZSAT
C 20 Dec 2004 Scott Hannon      Add PTYPE to call; fix error in NLAY
C                                  when PTYPE=AIRSLAY; add error trap
C                                  for LAT
C 18 May 2005 Scott Hannon      Add HNO3 based on SO2 code
C 23 Jun 2005 Scott Hannon      "trace" version for CO2,SO2,HNO3,N2O
C 23 Jan 2008 Scott Hannon      Add LCO2PM to allow CO2 profile in ppmv;
C                                  fix bug in CO2PPM check
C 24 Oct 2008 Scott Hannon      Update for RTP v2.01; emis & rho now
C                                  use the same freq points; set RHO
C                                  to (1-e)/pi if input < 0
C

!END====================================================================

C      =================================================================
       SUBROUTINE RDRTP(LWANT, IPROF, IOPCI,
     $    IH2O, IO3, ICO, ICH4, ICO2, ISO2, IHNO3, IN2O, PTYPE, RALT,
     $    LCO2PM,
     $    NLAY, NEMIS, LAT, LON, SATANG, SATZEN, ZSAT, SUNANG,
     $    PSURF, TSURF, CO2PPM, FEMIS, EMIS, RHO,
     $    TEMP, WAMNT, OAMNT, CAMNT, MAMNT, FAMNT, SAMNT, HAMNT, NAMNT,
     $    ALT, PROF, ISTAT )
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
       LOGICAL LWANT      ! do we want this profile?
       INTEGER IPROF      ! number of current profile
       INTEGER IOPCI      ! input RTP unit
       INTEGER IH2O       ! index of H2O in gamnt
       INTEGER IO3        ! index of O3 in gamnt
       INTEGER ICO        ! index of CO in gamnt
       INTEGER ICH4       ! index of CH4 in gamnt
       INTEGER ICO2       ! index of CO2 in gamnt
       INTEGER ISO2       ! index of SO2 in gamnt
       INTEGER IHNO3      ! index of HNO3 in gamnt
       INTEGER IN2O       ! index of N2O in gamnt
       INTEGER PTYPE      ! profile type code number
       REAL RALT(MAXLAY)  ! ref prof layer average altitudes
       LOGICAL LCO2PM     ! CO2 profile in ppmv?

C      Output parameters:
       INTEGER   NLAY
       INTEGER  NEMIS
       REAL    LAT
       REAL    LON
       REAL SATANG
       REAL SATZEN
       REAL   ZSAT
       REAL SUNANG
       REAL  PSURF
       REAL  TSURF
       REAL CO2PPM
       REAL  FEMIS(MXEMIS)
       REAL   EMIS(MXEMIS)
       REAL    RHO(MXEMIS)
       REAL   TEMP(MAXLAY)
       REAL  WAMNT(MAXLAY)
       REAL  OAMNT(MAXLAY)
       REAL  CAMNT(MAXLAY)
       REAL  MAMNT(MAXLAY)
       REAL  FAMNT(MAXLAY)
       REAL  SAMNT(MAXLAY)
       REAL  HAMNT(MAXLAY)
       REAL  NAMNT(MAXLAY)
       REAL    ALT(MAXLAY)
C
C      Profile data structure
       RECORD /RTPPROF/ PROF
C
       INTEGER ISTAT


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER I        ! generic
       INTEGER ICO2X    ! index for reading CO2 in gamnt
       INTEGER ISO2X    ! index for reading SO2 in gamnt
       INTEGER IHNOX    ! index for reading HNO3 in gamnt
       INTEGER IN2OX    ! index for reading N2O in gamnt
       INTEGER L        ! layer looping
       INTEGER LR       ! reversed layer looping
       INTEGER NLEV     ! number of levels
       INTEGER rtpread  ! for calling read rtp interface routine
       REAL ZSURF       ! surface altitude (read but ignored for now)
       REAL RJUNK1      ! generic junk/work
       REAL RJUNK2      ! generic junk/work

C-----------------------------------------------------------------------
C      SAVE STATEMENTS
C-----------------------------------------------------------------------
C      none


C***********************************************************************
C***********************************************************************
C      EXECUTABLE CODE begins below
C***********************************************************************
C***********************************************************************

C      ------------------------
C      Read the current profile
C      ------------------------
       ISTAT=rtpread(IOPCI, PROF)
C
       IF (ISTAT .EQ. -1) GOTO 9999  ! reached end of file
C
       IF (.NOT. LWANT) GOTO 9999    ! skip prof if not wanted
C
C      --------------------
C      Pull out needed data
C      --------------------
C      Latitude & longitude
       LAT=PROF.plat
       LON=PROF.plon
C
C      Can not correctly compute gravity if LAT is bad
       IF (ABS(LAT) .GT. 90.01) THEN
          WRITE(IOERR,1005) IPROF, LAT
 1005     FORMAT('ERROR! input profile PROF(',I4,').plat=',1PE11.4,
     $    ' is out of range -90 to +90')
          STOP
       ENDIF
C      Note: LON is not currently used and thus need not be checked
C
C      Number of levels
       NLEV=PROF.nlevs
       IF (NLEV .LT. 2 .OR. NLEV .GT. MAXLAY+1) THEN
          WRITE(IOERR,1010) IPROF, NLEV, MAXLAY+1
 1010     FORMAT('ERROR! input profile PROF(',I4,').nlevs=',I4,
     $    ' is out of range 2 to ',I3)
          STOP
       ENDIF
C
C      Number of layers
       IF (PTYPE .EQ. AIRSLAY) THEN
C         Special case for AIRS pseudo-levels
          NLAY=NLEV
       ELSE
          NLAY=NLEV - 1
       ENDIF
C
C      Assign read indices for trace gas CO2
       IF (ICO2 .LT. 1) THEN
          CO2PPM=PROF.co2ppm
          IF (CO2PPM .LT. -998) CO2PPM=CO2STD
          RJUNK1=0.8*CO2STD
          RJUNK2=1.2*CO2STD
          IF (CO2PPM .LT. RJUNK1 .OR. CO2PPM .GT. RJUNK2) THEN
             WRITE(IOERR,1015) IPROF, CO2PPM, RJUNK1, RJUNK2
 1015        FORMAT('Warning! PROF(',I4,').co2ppm=',1PE10.3,
     $       ' is outside allowed range ',0PF5.1,' to ',F5.1)
          ENDIF
C         Set ICO2X to any valid gas index; data will be read but ignored
          ICO2X=IH2O
       ELSE
          CO2PPM=-9999
          ICO2X=ICO2
       ENDIF
C      Assign read indices for trace gas SO2
       IF (ISO2 .LT. 1) THEN
          ISO2X=IH2O
       ELSE
          ISO2X=ISO2
       ENDIF
C      Assign read indices for trace gas HNO3
       IF (IHNO3 .LT. 1) THEN
          IHNOX=IH2O
       ELSE
          IHNOX=IHNO3
       ENDIF
C      Assign read indices for trace gas N2O
       IF (IN2O .LT. 1) THEN
          IN2OX=IH2O
       ELSE
          IN2OX=IN2O
       ENDIF
C
C      Angles
       SUNANG=PROF.solzen
       SATANG=PROF.scanang
       SATZEN=PROF.satzen
C
C      Satellite altitude above ellipsoid surface (convert m to km)
       ZSAT=PROF.zobs/1000
C
C      Surface
       PSURF=PROF.spres
       TSURF=PROF.stemp
       ZSURF=PROF.salti  ! note: ZSURF is currently ignored
       IF (PSURF .LE. 0) THEN
          WRITE(IOERR,1017) IPROF
 1017     FORMAT('ERROR! Prof(',I4,') has no surface pressure')
          STOP
       ENDIF
       IF (TSURF .LE. 0) THEN
          WRITE(IOERR,1018) IPROF
 1018     FORMAT('ERROR! Prof(',I4,') has no surface temperature')
          STOP
       ENDIF
C
C      Emissivity (range 0 to 1) and Reflectance (range 0 to 1/pi)
       NEMIS=PROF.nemis
       IF (NEMIS .EQ. 0) THEN
          WRITE(IOERR,1020) IPROF
 1020     FORMAT('ERROR! PROF(',I4,') has no emissivity')
          STOP
       ENDIF
       DO I=1,NEMIS
          FEMIS(I)=PROF.efreq(I)
          EMIS(I)=PROF.emis(I)
          IF (PROF.rho(I) .LT. 0.0) THEN
             RHO(I)=(1 - EMIS(I))/PI
          ELSE
             RHO(I)=PROF.rho(I)
          ENDIF
       ENDDO
C
C      ----------------------------------
C      Get layer temperature & gas amount
C      ----------------------------------
       IF (GUCIN .EQ. 1) THEN
C         Input gas units are molecules/cm^2; convert to kilomoles/cm^2
          IF (PROF.plevs(1) .LT. PROF.plevs(NLEV)) THEN
C            Prof is in top-down order
             DO L=1,NLAY
                TEMP(L)=PROF.ptemp(L)
                WAMNT(L)=PROF.gamnt(L,IH2O )/6.02214199E+26
                OAMNT(L)=PROF.gamnt(L,IO3  )/6.02214199E+26
                CAMNT(L)=PROF.gamnt(L,ICO  )/6.02214199E+26
                MAMNT(L)=PROF.gamnt(L,ICH4 )/6.02214199E+26
                FAMNT(L)=PROF.gamnt(L,ICO2X)/6.02214199E+26
                SAMNT(L)=PROF.gamnt(L,ISO2X)/6.02214199E+26
                HAMNT(L)=PROF.gamnt(L,IHNOX)/6.02214199E+26
                NAMNT(L)=PROF.gamnt(L,IN2OX)/6.02214199E+26
                ALT(L)=0.5*( PROF.palts(L) + PROF.palts(L+1) )
             ENDDO
             IF (LCO2PM) THEN
                DO L=1,NLAY
                   FAMNT(L)=PROF.gamnt(L,ICO2X)
                ENDDO
             ENDIF
          ELSE
C            Prof is in bottom-up order
             DO L=1,NLAY
                LR=1 + NLAY - L  ! reversed layer index
                TEMP(L)=PROF.ptemp(LR)
                WAMNT(L)=PROF.gamnt(LR,IH2O )/6.02214199E+26
                OAMNT(L)=PROF.gamnt(LR,IO3  )/6.02214199E+26
                CAMNT(L)=PROF.gamnt(LR,ICO  )/6.02214199E+26
                MAMNT(L)=PROF.gamnt(LR,ICH4 )/6.02214199E+26
                FAMNT(L)=PROF.gamnt(LR,ICO2X)/6.02214199E+26
                SAMNT(L)=PROF.gamnt(LR,ISO2X)/6.02214199E+26
                HAMNT(L)=PROF.gamnt(LR,IHNOX)/6.02214199E+26
                NAMNT(L)=PROF.gamnt(LR,IN2OX)/6.02214199E+26
                ALT(L)=0.5*( PROF.palts(LR) + PROF.palts(LR+1) )
             ENDDO
             IF (LCO2PM) THEN
                DO L=1,NLAY
                   LR=1 + NLAY - L  ! reversed layer index
                   FAMNT(L)=PROF.gamnt(LR,ICO2X)
                ENDDO
             ENDIF
          ENDIF
C
       ELSEIF (GUCIN .EQ. 2) THEN
C         Input gas units are kilomoles/cm^2
          IF (PROF.plevs(1) .LT. PROF.plevs(NLEV)) THEN
C            Prof is in top-down order
             DO L=1,NLAY
                TEMP(L)=PROF.ptemp(L)
                WAMNT(L)=PROF.gamnt(L,IH2O)
                OAMNT(L)=PROF.gamnt(L,IO3)
                CAMNT(L)=PROF.gamnt(L,ICO)
                MAMNT(L)=PROF.gamnt(L,ICH4)
                FAMNT(L)=PROF.gamnt(L,ICO2X)
                SAMNT(L)=PROF.gamnt(L,ISO2X)
                HAMNT(L)=PROF.gamnt(L,IHNOX)
                NAMNT(L)=PROF.gamnt(L,IN2OX)
                ALT(L)=0.5*( PROF.palts(L) + PROF.palts(L+1) )
             ENDDO
          ELSE
C            Prof is in bottom-up order
             DO L=1,NLAY
                LR=1 + NLAY - L  ! reversed layer index
                TEMP(L)=PROF.ptemp(LR)
                WAMNT(L)=PROF.gamnt(LR,IH2O)
                OAMNT(L)=PROF.gamnt(LR,IO3)
                CAMNT(L)=PROF.gamnt(LR,ICO)
                MAMNT(L)=PROF.gamnt(LR,ICH4)
                FAMNT(L)=PROF.gamnt(LR,ICO2X)
                SAMNT(L)=PROF.gamnt(LR,ISO2X)
                HAMNT(L)=PROF.gamnt(LR,IHNOX)
                NAMNT(L)=PROF.gamnt(LR,IN2OX)
                ALT(L)=0.5*( PROF.palts(LR) + PROF.palts(LR+1) )
             ENDDO
          ENDIF
       ELSE ! empty else
       ENDIF
C

C      -----------------
C      Default altitudes
C      -----------------
       IF (PROF.palts(1) .LT. -998 .OR. PTYPE .EQ. AIRSLAY) THEN
C         No altitudes, use reference
          DO L=1,NLAY
             ALT(L)=RALT(L)
          ENDDO
       ENDIF
c
c for now ignore clouds
c

C
 9999  RETURN
       END
