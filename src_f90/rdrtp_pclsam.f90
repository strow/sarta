! version for sarta
 
! Code converted using TO_F90_LOOP by Alan Miller
! Date: 2023-04-04  Time: 16:44:56
 
!=======================================================================
!=======================================================================

!              University of Maryland Baltimore County [UMBC]

!              AIRS

!              RDRTP version with trace gases

!F77====================================================================


!ROUTINE NAME: RDRTP


!ABSTRACT:
!    Read a profile from a previously openned RTP file


!CALL PROTOCOL:
!    RDRTP( LWANT, IPROF, IOPCI,
!       IH2O, IO3, ICO, ICH4, ICO2, ISO2, IHNO3, IN2O,
!       PTYPE, RALT,LCO2PM,NLAY, NEMIS, LAT, LON, SATANG, SATZEN,
!       ZSAT, SUNANG, COSDAZ, PSURF, TSURF, CO2PPM,
!       FEMIS, EMIS, RHO,
!       TEMP, WAMNT, OAMNT, CAMNT, MAMNT, FAMNT, SAMNT, HAMNT, NAMNT,
!       ALT, PROF, ISTAT )


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
!    REAL      COSDAZ  cosine of delta azimuth     none
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
!    REAL arr  ALT     layer average altitude      meters
!    STRUCT    PROF    RTP profile structure       various
!    INTEGER   ISTAT   I/O status                  none
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
!------------ ----------------- ----------------------------------------
! 14 Feb 2001 Scott Hannon      created based on klayers version
! 13 Sep 2001 Scott Hannon      Added checks of PSURF & TSURF
! 31 Oct 2002 Scott Hannon      Add output vars SATZEN and ZSAT
! 20 Dec 2004 Scott Hannon      Add PTYPE to call; fix error in NLAY
!                                  when PTYPE=AIRSLAY; add error trap
!                                  for LAT
! 18 May 2005 Scott Hannon      Add HNO3 based on SO2 code
! 23 Jun 2005 Scott Hannon      "trace" version for CO2,SO2,HNO3,N2O
! 23 Jan 2008 Scott Hannon      Add LCO2PM to allow CO2 profile in ppmv;
!                                  fix bug in CO2PPM check
! 24 Mar 2008 Scott Hannon      Add COSDAZ & related code
! 24 Nov 2008 Scott Hannon      Update for rtpV201

!END====================================================================

!      =================================================================

SUBROUTINE RDRTP(LWANT, IPROF, IOPCI,  &
    IH2O, IO3, ICO, ICH4, ICO2, ISO2, IHNO3, IN2O, PTYPE, RALT,  &
    LCO2PM, NLAY, NEMIS, LAT, LON, SATANG, SATZEN, ZSAT, SUNANG, COSDAZ,  &
    PSURF, TSURF, CO2PPM, FEMIS, EMIS, RHO,  &
    TEMP, WAMNT, OAMNT, CAMNT, MAMNT, FAMNT, SAMNT, HAMNT, NAMNT,  &
    ALT, PROF, ISTAT )
!      =================================================================


!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------

LOGICAL, INTENT(IN OUT)                  :: LWANT
INTEGER, INTENT(IN OUT)                  :: IPROF
INTEGER, INTENT(IN OUT)                  :: IOPCI
INTEGER, INTENT(IN)                      :: IH2O
INTEGER, INTENT(IN)                      :: IO3
INTEGER, INTENT(IN)                      :: ICO
INTEGER, INTENT(IN)                      :: ICH4
INTEGER, INTENT(IN)                      :: ICO2
INTEGER, INTENT(IN)                      :: ISO2
INTEGER, INTENT(IN)                      :: IHNO3
INTEGER, INTENT(IN)                      :: IN2O
INTEGER, INTENT(IN)                      :: PTYPE
REAL, INTENT(IN)                         :: RALT(MAXLAY)
LOGICAL, INTENT(IN OUT)                  :: LCO2PM
INTEGER, INTENT(OUT)                     :: NLAY
INTEGER, INTENT(OUT)                     :: NEMIS
REAL, INTENT(OUT)                        :: LAT
REAL, INTENT(OUT)                        :: LON
REAL, INTENT(OUT)                        :: SATANG
REAL, INTENT(OUT)                        :: SATZEN
REAL, INTENT(OUT)                        :: ZSAT
REAL, INTENT(OUT)                        :: SUNANG
REAL, INTENT(OUT)                        :: COSDAZ
REAL, INTENT(OUT)                        :: PSURF
REAL, INTENT(OUT)                        :: TSURF
REAL, INTENT(OUT)                        :: CO2PPM
REAL, INTENT(OUT)                        :: FEMIS(MXEMIS)
REAL, INTENT(OUT)                        :: EMIS(MXEMIS)
REAL, INTENT(OUT)                        :: RHO(MXEMIS)
REAL, INTENT(OUT)                        :: TEMP(MAXLAY)
REAL, INTENT(OUT)                        :: WAMNT(MAXLAY)
REAL, INTENT(OUT)                        :: OAMNT(MAXLAY)
REAL, INTENT(OUT)                        :: CAMNT(MAXLAY)
REAL, INTENT(OUT)                        :: MAMNT(MAXLAY)
REAL, INTENT(OUT)                        :: FAMNT(MAXLAY)
REAL, INTENT(OUT)                        :: SAMNT(MAXLAY)
REAL, INTENT(OUT)                        :: HAMNT(MAXLAY)
REAL, INTENT(OUT)                        :: NAMNT(MAXLAY)
REAL, INTENT(OUT)                        :: ALT(MAXLAY)
NO TYPE, INTENT(OUT)                     :: PROF
INTEGER, INTENT(OUT)                     :: ISTAT
IMPLICIT NONE


!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
INCLUDE 'incFTC.f'
INCLUDE 'rtpdefs.f'


!-----------------------------------------------------------------------
!      EXTERNAL FUNCTIONS
!-----------------------------------------------------------------------
!      none


!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
!      Input parameters:












REAL :: ! ref prof layer average altitudes


!      Output parameters:


























!      Profile data structure
RECORD /RTPPROF/ PROF




!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
INTEGER :: I        ! generic
INTEGER :: ICO2X    ! index for reading CO2 in gamnt
INTEGER :: ISO2X    ! index for reading SO2 in gamnt
INTEGER :: IHNOX    ! index for reading HNO3 in gamnt
INTEGER :: IN2OX    ! index for reading N2O in gamnt
INTEGER :: L        ! layer looping
INTEGER :: LR       ! reversed layer looping
INTEGER :: NLEV     ! number of levels
INTEGER :: rtpread  ! for calling read rtp interface routine
REAL :: ZSURF       ! surface altitude (read but ignored for now)
REAL :: RJUNK1      ! generic junk/work
REAL :: RJUNK2      ! generic junk/work

REAL :: CONV        ! degreees to radians conversion factor

!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none


!***********************************************************************
!***********************************************************************
!      EXECUTABLE CODE begins below
!***********************************************************************
!***********************************************************************

!      CONV = pi/180 = degrees to radians conversion factor
CONV=1.7453292E-02

!      ------------------------
!      Read the current profile
!      ------------------------
ISTAT=rtpread(IOPCI, PROF)

IF (ISTAT == -1) GO TO 9999  ! reached end of file

IF (.NOT. LWANT) GO TO 9999    ! skip prof if not wanted

!      --------------------
!      Pull out needed data
!      --------------------
!      Latitude & longitude
LAT=PROF%plat
LON=PROF%plon

!      Can not correctly compute gravity if LAT is bad
IF (ABS(LAT) > 90.01) THEN
  WRITE(IOERR,1005) IPROF, LAT
  1005     FORMAT('ERROR! input profile PROF(',I4,').plat=',1PE11.4,  &
      ' is out of range -90 to +90')
  STOP
END IF
!      Note: LON is not currently used and thus need not be checked

!      Number of levels
NLEV=PROF%nlevs
IF (NLEV < 2 .OR. NLEV > MAXLAY+1) THEN
  WRITE(IOERR,1010) IPROF, NLEV, MAXLAY+1
  1010     FORMAT('ERROR! input profile PROF(',I4,').nlevs=',I4,  &
      ' is out of range 2 to ',I3)
  STOP
END IF

!      Number of layers
IF (PTYPE == AIRSLAY) THEN
!         Special case for AIRS pseudo-levels
  NLAY=NLEV
ELSE
  NLAY=NLEV - 1
END IF

!      Assign read indices for trace gas CO2
IF (ICO2 < 1) THEN
  CO2PPM=PROF%co2ppm
  IF (CO2PPM < -998) CO2PPM=CO2STD
  RJUNK1=0.8*CO2STD
  RJUNK2=1.2*CO2STD
  IF (CO2PPM < RJUNK1 .OR. CO2PPM > RJUNK2) THEN
    WRITE(IOERR,1015) IPROF, CO2PPM, RJUNK1, RJUNK2
    1015        FORMAT('Warning! PROF(',I4,').co2ppm=',1PE10.3,  &
        ' is outside allowed range ',0PF5.1,' to ',F5.1)
  END IF
!         Set ICO2X to any valid gas index; data will be read but ignored
  ICO2X=IH2O
ELSE
  CO2PPM=-9999
  ICO2X=ICO2
END IF
!      Assign read indices for trace gas SO2
IF (ISO2 < 1) THEN
  ISO2X=IH2O
ELSE
  ISO2X=ISO2
END IF
!      Assign read indices for trace gas HNO3
IF (IHNO3 < 1) THEN
  IHNOX=IH2O
ELSE
  IHNOX=IHNO3
END IF
!      Assign read indices for trace gas N2O
IF (IN2O < 1) THEN
  IN2OX=IH2O
ELSE
  IN2OX=IN2O
END IF

!      Angles
SUNANG=PROF%solzen
SATANG=PROF%scanang
SATZEN=PROF%satzen

!      Azimuth angles
RJUNK1=PROF%satazi
RJUNK2=PROF%solazi
IF (RJUNK1 < -180.0) RJUNK1=-180.0
IF (RJUNK2 < -180.0) RJUNK2=-180.0
COSDAZ=COS( (RJUNK1-RJUNK2)*CONV )

!      Satellite altitude above ellipsoid surface (convert m to km)
ZSAT=PROF%zobs/1000

!      Surface
PSURF=PROF%spres
TSURF=PROF%stemp
ZSURF=PROF%salti  ! note: ZSURF is currently ignored
IF (PSURF <= 0) THEN
  WRITE(IOERR,1017) IPROF
  1017     FORMAT('ERROR! Prof(',I4,') has no surface pressure')
  STOP
END IF
IF (TSURF <= 0) THEN
  WRITE(IOERR,1018) IPROF
  1018     FORMAT('ERROR! Prof(',I4,') has no surface temperature')
  STOP
END IF

!      Emissivity (range 0 to 1) and reflectance
NEMIS=PROF%nemis
IF (NEMIS == 0) THEN
  WRITE(IOERR,1020) IPROF
  1020     FORMAT('ERROR! PROF(',I4,') has no emissivity')
  STOP
END IF
DO I=1,NEMIS
!         WARNING! does not check if emis is ok
  FEMIS(I)=PROF%efreq(I)
  EMIS(I)=PROF%emis(I)
  IF (PROF%rho(I) < 0.0) THEN
    RHO(I)=(1 - EMIS(I))/PI
  ELSE
    RHO(I)=PROF%rho(I)
  END IF
  ENDDO
    
!      ----------------------------------
!      Get layer temperature & gas amount
!      ----------------------------------
    IF (GUCIN == 1) THEN
!         Input gas units are molecules/cm^2; convert to kilomoles/cm^2
      IF (PROF%plevs(1) < PROF%plevs(NLEV)) THEN
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
          ALT(L)=0.5*( PROF%palts(L) + PROF%palts(L+1) )
          ENDDO
            IF (LCO2PM) THEN
              DO L=1,NLAY
                FAMNT(L)=PROF%gamnt(L,ICO2X)
                ENDDO
                END IF
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
                  ALT(L)=0.5*( PROF%palts(LR) + PROF%palts(LR+1) )
                  ENDDO
                    IF (LCO2PM) THEN
                      DO L=1,NLAY
                        LR=1 + NLAY - L  ! reversed layer index
                        FAMNT(L)=PROF%gamnt(LR,ICO2X)
                        ENDDO
                        END IF
                      END IF
                      
                    ELSE IF (GUCIN == 2) THEN
!         Input gas units are kilomoles/cm^2
                      IF (PROF%plevs(1) < PROF%plevs(NLEV)) THEN
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
                              ALT(L)=0.5*( PROF%palts(LR) + PROF%palts(LR+1) )
                              ENDDO
                              END IF
                            ELSE ! empty else
                            END IF
                            
                            
!      -----------------
!      Default altitudes
!      -----------------
                            IF (PROF%palts(1) < -998 .OR. PTYPE == AIRSLAY) THEN
!         No altitudes, use reference
                              DO L=1,NLAY
                                ALT(L)=RALT(L)
                                ENDDO
                                END IF
                                
! for now ignore clouds
                                
                                
                                
                                9999  RETURN
                              END SUBROUTINE RDRTP
