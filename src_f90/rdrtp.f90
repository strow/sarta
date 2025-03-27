! version for sarta
!=======================================================================
!=======================================================================
!
!              University of Maryland Baltimore County [UMBC]
!
!              AIRS
!
!              RDRTP version with trace gases and NH3
!
!F77====================================================================


!ROUTINE NAME: RDRTP


!ABSTRACT:
!    Read a profile from a previously openned RTP file


!CALL PROTOCOL:
!    RDRTP( LWANT, IPROF, IOPCI,
!       IH2O, IO3, ICO, ICH4, ICO2, ISO2, IHNO3, IN2O, INH3,
!       PTYPE, RALT,LCO2PM,NLAY, NEMIS, LAT, LON, SATANG, SATZEN,
!       ZSAT, SUNANG, PSURF, TSURF, CO2PPM, FEMIS, EMIS, RHO,
!       TEMP, WAMNT, OAMNT, CAMNT, MAMNT, FAMNT, SAMNT, HAMNT, NAMNT,
!       AAMNT, ALT, PROF, ISTAT )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    LOGICAL   LWANT   do you want this profile?   none
!    INTEGER   IPROF   profile number              none
!    INTEGER   IOPCI   input RTP file I/O number   none
!    INTEGER   IH2O    index of H2O in gamnt       none
!    INTEGER   IO3     index of O3 in gamnt        none
!    INTEGER   ICO     index of CO in gamnt        none
!    INTEGER   ICH4    index of CH4 in gamnt       none
!    INTEGER   ICO2    index of CO2 in gamnt       none
!    INTEGER   ISO2    index of SO2 in gamnt       none
!    INTEGER   IHNO3   index of HNO3 in gamnt      none
!    INTEGER   IN2O    index of N2O in gamnt       none
!    INTEGER   INH3    index of NH3 in gamnt       none
!    INTEGER   PTYPE   profile type code number    none
!    REAL arr  RALT    ref prof layer altitudes    meters
!    LOGICAL   LCO2PM  CO2 profile in ppmv?        none


!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INTEGER   NLAY    number of used layers       none
!    INTEGER   NEMIS   number of used emis pts     none
!    REAL      LAT     latitude                    degrees
!    REAL      LON     longitude                   degrees
!    REAL      SATANG  satellite scan angle        degrees
!    REAL      SATZEN  satellite zenith angle      degrees
!    REAL      ZSAT    satellite altitude          kilometer
!    REAL      SUNANG  sun zenith angle            degrees
!    REAL      PSURF   surface pressure            millibars
!    REAL      TSURF   surface skin temperature    Kelvin
!    REAL      CO2PPM  mean trop-strat CO2 mix rat PPMV
!    REAL arr  FEMIS   emis freq points            cm^-1
!    REAL arr  EMIS    emis points                 none (0 to 1)
!    REAL arr  RHO     rho points                  none (0 to 1/pi)
!    REAL arr  TEMP    layer temperature           Kelvin
!    REAL arr  WAMNT   layer Water vapor amount    */cm^2
!    REAL arr  OAMNT   layer Ozone amount          */cm^2
!    REAL arr  CAMNT   layer CO amount             */cm^2
!    REAL arr  MAMNT   layer Methane amount        */cm^2
!    REAL arr  FAMNT   layer CO2 amount            */cm^2
!    REAL arr  SAMNT   layer SO2 amount            */cm^2
!    REAL arr  HAMNT   layer HNO3 amount           */cm^2
!    REAL arr  NAMNT   layer N2O amount            */cm^2
!    REAL arr  AAMNT   layer NH3 amount            */cm^2
!    REAL arr  ALT     layer average altitude      meters
!    STRUCT    PROF    RTP profile structure       various
!    INTEGER   ISTAT   I/O status                  none
!    REAL      HDODPL  HDO depletion as per.mil    o/oo
! note: units "*/cm^2" can be either kilomoles/cm^2 or molecules/cm^2


!INPUT/OUTPUT PARAMETERS: none


!RETURN VALUES: none


!PARENT(S): KLAYERS


!ROUTINES CALLED: none


!FILES ACCESSED:
!    Input RTP file withI/O number IOPCI
!    unit IOERR: error messages
!    unit IOINFO: info/warning messages


!COMMON BLOCKS: none


!DESCRIPTION:
!    Reads a single profile from a previously openned RTP file.
!    The routine expects to find the data specified in the header
!    of the input RTP file.


!ALGORITHM REFERENCES: see DESCRIPTION


!KNOWN BUGS AND LIMITATIONS:
!    none


!ROUTINE HISTORY:
!    Date     Programmer        Comments
!------------ --------------- ----------------------------------------
! 14 Feb 2001 Scott Hannon    created based on klayers version
! 13 Sep 2001 Scott Hannon    Added checks of PSURF & TSURF
! 31 Oct 2002 Scott Hannon    Add output vars SATZEN and ZSAT
! 20 Dec 2004 Scott Hannon    Add PTYPE to call; fix error in NLAY
!                                when PTYPE=AIRSLAY; add error trap
!                                for LAT
! 18 May 2005 Scott Hannon    Add HNO3 based on SO2 code
! 23 Jun 2005 Scott Hannon    "trace" version for CO2,SO2,HNO3,N2O
! 23 Jan 2008 Scott Hannon    Add LCO2PM to allow CO2 profile in ppmv;
!                                fix bug in CO2PPM check
! 24 Oct 2008 Scott Hannon    Update for RTP v2.01; emis & rho now
!                                use the same freq points; set RHO
!                                to (1-e)/pi if input < 0
!
! 10 May 2018 C Hepplewhite   Add NH3
! 22 Mar 2023 C Hepplewhite   Add HDODPL from PROF%udef for HDO
!                             depletion

!END====================================================================

!      =================================================================
       SUBROUTINE RDRTP(LWANT, IPROF, IOPCI, &
         IH2O, IO3, ICO, ICH4, ICO2, ISO2, IHNO3, IN2O, INH3, &
         PTYPE, RALT, LCO2PM, &
         NLAY,  NEMIS, LAT,    LON,    SATANG, SATZEN, ZSAT, SUNANG, &
         PSURF, TSURF, CO2PPM, HDODPL, FEMIS,  EMIS,   RHO, &
         TEMP,  WAMNT, OAMNT,  CAMNT,  MAMNT,  FAMNT,  SAMNT, HAMNT, &
         NAMNT, AAMNT, ALT, PROF, ISTAT )
!      =================================================================


use incFTC
!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------
       IMPLICIT NONE

!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
include 'rtpdefs.f90'


!-----------------------------------------------------------------------
!      EXTERNAL FUNCTIONS
!-----------------------------------------------------------------------
!      none


!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
!      Input parameters:
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
       INTEGER INH3       ! index of NH3 in gamnt
       INTEGER PTYPE      ! profile type code number
       REAL RALT(MAXLAY)  ! ref prof layer average altitudes
       LOGICAL LCO2PM     ! CO2 profile in ppmv?

!      Output parameters:
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
       REAL HDODPL 
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
       REAL  AAMNT(MAXLAY)
       REAL    ALT(MAXLAY)
!
!      Profile data structure
       RECORD /RTPPROF/ PROF
!
       INTEGER ISTAT


!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
       INTEGER I        ! generic
       INTEGER ICO2X    ! index for reading CO2 in gamnt
       INTEGER ISO2X    ! index for reading SO2 in gamnt
       INTEGER IHNOX    ! index for reading HNO3 in gamnt
       INTEGER IN2OX    ! index for reading N2O in gamnt
       INTEGER INH3X    ! index for reading NH3 in gamnt
       INTEGER L        ! layer looping
       INTEGER LR       ! reversed layer looping
       INTEGER NLEV     ! number of levels
       INTEGER rtpread  ! for calling read rtp interface routine
       REAL ZSURF       ! surface altitude (read but ignored for now)
       REAL RJUNK1      ! generic junk/work
       REAL RJUNK2      ! generic junk/work
!       REAL UDEF(MAXUDEF)        ! PROF%udef entry 20 for HDODPL

!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none


!***********************************************************************
!***********************************************************************
!      EXECUTABLE CODE begins below
!***********************************************************************
!***********************************************************************
!      ------------------------
!      Read the current profile
!      ------------------------
      if (DEBUG) write(6,2010) LWANT,IPROF
 2010 FORMAT('in RDRTP: IPROF: ', L2,X,I5)
!
       ISTAT=rtpread(IOPCI, PROF)
!
       IF (ISTAT .EQ. -1) GOTO 9999  ! reached end of file
!
       IF (.NOT. LWANT) GOTO 9999    ! skip prof if not wanted
!
!      --------------------
!      Pull out needed data
!      --------------------
!      Latitude & longitude
       LAT=PROF%plat
       LON=PROF%plon
!
!      Can not correctly compute gravity if LAT is bad
       IF (ABS(LAT) .GT. 90.01) THEN
          WRITE(IOERR,1005) IPROF, LAT
 1005     FORMAT('ERROR! input profile PROF(',I4,').plat=',1PE11.4, &
         ' is out of range -90 to +90')
          STOP
       ENDIF
!      Note: LON is not currently used and thus need not be checked
!
!      Number of levels
       NLEV=PROF%nlevs
       IF (NLEV .LT. 2 .OR. NLEV .GT. MAXLAY+1) THEN
          WRITE(IOERR,1010) IPROF, NLEV, MAXLAY+1
 1010     FORMAT('ERROR! input profile PROF(',I4,').nlevs=',I4, &
         ' is out of range 2 to ',I3)
          STOP
       ENDIF
!
!      Number of layers
       IF (PTYPE .EQ. AIRSLAY) THEN
!         Special case for AIRS pseudo-levels
          NLAY=NLEV
       ELSE
          NLAY=NLEV - 1
       ENDIF
!
!      Assign read indices for trace gas CO2
       IF (ICO2 .LT. 1) THEN
          CO2PPM=PROF%co2ppm
          IF (CO2PPM .LT. -998) CO2PPM=CO2STD
          RJUNK1=0.8*CO2STD
          RJUNK2=1.2*CO2STD
          IF (CO2PPM .LT. RJUNK1 .OR. CO2PPM .GT. RJUNK2) THEN
             WRITE(IOERR,1015) IPROF, CO2PPM, RJUNK1, RJUNK2
 1015        FORMAT('Warning! PROF(',I4,').co2ppm=',1PE10.3, &
            ' is outside allowed range ',0PF5.1,' to ',F5.1)
          ENDIF
!         Set ICO2X to any valid gas index; data will be read but ignored
          ICO2X=IH2O
       ELSE
          CO2PPM=-9999
          ICO2X=ICO2
       ENDIF
!      Assign read indices for trace gas SO2
       IF (ISO2 .LT. 1) THEN
          ISO2X=IH2O
       ELSE
          ISO2X=ISO2
       ENDIF
!      Assign read indices for trace gas HNO3
       IF (IHNO3 .LT. 1) THEN
          IHNOX=IH2O
       ELSE
          IHNOX=IHNO3
       ENDIF
!      Assign read indices for trace gas N2O
       IF (IN2O .LT. 1) THEN
          IN2OX=IH2O
       ELSE
          IN2OX=IN2O
       ENDIF
!      Assign read indices for trace gas NH3
       IF (INH3 .LT. 1) THEN
          INH3X=IH2O
       ELSE
          INH3X=INH3
       ENDIF
!
!      Angles
       SUNANG=PROF%solzen
       SATANG=PROF%scanang
       SATZEN=PROF%satzen
!
!      Satellite altitude above ellipsoid surface (convert m to km)
       ZSAT=PROF%zobs/1000
!
!      Surface
       PSURF=PROF%spres
       TSURF=PROF%stemp
       ZSURF=PROF%salti  ! note: ZSURF is currently ignored
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
!
!      Emissivity (range 0 to 1) and Reflectance (range 0 to 1/pi)
       NEMIS=PROF%nemis
       IF (NEMIS .EQ. 0) THEN
          WRITE(IOERR,1020) IPROF
 1020     FORMAT('ERROR! PROF(',I4,') has no emissivity')
          STOP
       ENDIF
       DO I=1,NEMIS
          FEMIS(I)=PROF%efreq(I)
          EMIS(I)=PROF%emis(I)
          IF (PROF%rho(I) .LT. 0.0) THEN
             RHO(I)=(1 - EMIS(I))/PI
          ELSE
             RHO(I)=PROF%rho(I)
          ENDIF
       ENDDO
!
!      HDO Depletion from UDEF(20,:)
       HDODPL=PROF%udef(20)
  if(DEBUG) write(6,"('rdrtp:HDODPL= ',F8.1)"),HDODPL
!
!      ----------------------------------
!      Get layer temperature & gas amount
!      ----------------------------------
       IF (GUCIN .EQ. 1) THEN
!         Input gas units are molecules/cm^2; convert to kilomoles/cm^2
          IF (PROF%plevs(1) .LT. PROF%plevs(NLEV)) THEN
!            Prof is in top-down order
             DO L=1,NLAY
                TEMP(L)=PROF%ptemp(L)
                WAMNT(L)=PROF%gamnt(L,IH2O )/6.02214199E+26
                OAMNT(L)=PROF%gamnt(L,IO3  )/6.02214199E+26
                CAMNT(L)=PROF%gamnt(L,ICO  )/6.02214199E+26
                MAMNT(L)=PROF%gamnt(L,ICH4 )/6.02214199E+26
                FAMNT(L)=PROF%gamnt(L,ICO2X)/6.02214199E+26
                SAMNT(L)=PROF%gamnt(L,ISO2X)/6.02214199E+26
                HAMNT(L)=PROF%gamnt(L,IHNOX)/6.02214199E+26
                NAMNT(L)=PROF%gamnt(L,IN2OX)/6.02214199E+26
                AAMNT(L)=PROF.gamnt(L,INH3X)/6.02214199E+26
                ALT(L)=0.5*( PROF%palts(L) + PROF%palts(L+1) )
             ENDDO
             IF (LCO2PM) THEN
                DO L=1,NLAY
                   FAMNT(L)=PROF%gamnt(L,ICO2X)
                ENDDO
             ENDIF
          ELSE
!            Prof is in bottom-up order
             DO L=1,NLAY
                LR=1 + NLAY - L  ! reversed layer index
                TEMP(L)=PROF%ptemp(LR)
                WAMNT(L)=PROF%gamnt(LR,IH2O )/6.02214199E+26
                OAMNT(L)=PROF%gamnt(LR,IO3  )/6.02214199E+26
                CAMNT(L)=PROF%gamnt(LR,ICO  )/6.02214199E+26
                MAMNT(L)=PROF%gamnt(LR,ICH4 )/6.02214199E+26
                FAMNT(L)=PROF%gamnt(LR,ICO2X)/6.02214199E+26
                SAMNT(L)=PROF%gamnt(LR,ISO2X)/6.02214199E+26
                HAMNT(L)=PROF%gamnt(LR,IHNOX)/6.02214199E+26
                NAMNT(L)=PROF%gamnt(LR,IN2OX)/6.02214199E+26
                AAMNT(L)=PROF.gamnt(LR,INH3X)/6.02214199E+26
               ALT(L)=0.5*( PROF%palts(LR) + PROF%palts(LR+1) )
             ENDDO
             IF (LCO2PM) THEN
                DO L=1,NLAY
                   LR=1 + NLAY - L  ! reversed layer index
                   FAMNT(L)=PROF%gamnt(LR,ICO2X)
                ENDDO
             ENDIF
          ENDIF
!
       ELSEIF (GUCIN .EQ. 2) THEN
!         Input gas units are kilomoles/cm^2
          IF (PROF%plevs(1) .LT. PROF%plevs(NLEV)) THEN
!            Prof is in top-down order
             DO L=1,NLAY
                TEMP(L)=PROF%ptemp(L)
                WAMNT(L)=PROF%gamnt(L,IH2O)
                OAMNT(L)=PROF%gamnt(L,IO3)
                CAMNT(L)=PROF%gamnt(L,ICO)
                MAMNT(L)=PROF%gamnt(L,ICH4)
                FAMNT(L)=PROF%gamnt(L,ICO2X)
                SAMNT(L)=PROF%gamnt(L,ISO2X)
                HAMNT(L)=PROF%gamnt(L,IHNOX)
                NAMNT(L)=PROF%gamnt(L,IN2OX)
                AAMNT(L)=PROF.gamnt(L,INH3X)
                ALT(L)=0.5*( PROF%palts(L) + PROF%palts(L+1) )
             ENDDO
          ELSE
!            Prof is in bottom-up order
             DO L=1,NLAY
                LR=1 + NLAY - L  ! reversed layer index
                TEMP(L)=PROF%ptemp(LR)
                WAMNT(L)=PROF%gamnt(LR,IH2O)
                OAMNT(L)=PROF%gamnt(LR,IO3)
                CAMNT(L)=PROF%gamnt(LR,ICO)
                MAMNT(L)=PROF%gamnt(LR,ICH4)
                FAMNT(L)=PROF%gamnt(LR,ICO2X)
                SAMNT(L)=PROF%gamnt(LR,ISO2X)
                HAMNT(L)=PROF%gamnt(LR,IHNOX)
                NAMNT(L)=PROF%gamnt(LR,IN2OX)
                AAMNT(L)=PROF.gamnt(LR,INH3X)
                ALT(L)=0.5*( PROF%palts(LR) + PROF%palts(LR+1) )
             ENDDO
          ENDIF
       ELSE ! empty else
       ENDIF
!
!      -----------------
!      Default altitudes
!      -----------------
       IF (PROF%palts(1) .LT. -998 .OR. PTYPE .EQ. AIRSLAY) THEN
!         No altitudes, use reference
          DO L=1,NLAY
             ALT(L)=RALT(L)
          ENDDO
       ENDIF
!
! for now ignore clouds
!

!
 9999  RETURN
       END
