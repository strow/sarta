! This version with rtp input profiles and command-line arguments
 
! Code converted using TO_F90_LOOP by Alan Miller
! Date: 2023-04-04  Time: 16:44:56
 
!=======================================================================
!=======================================================================

!    University of Maryland Baltimore County (UMBC)

!    AIRS

!    SARTA version with trace gases

!F77====================================================================


!ROUTINE NAME:
!    SARTA


!ABSTRACT:
!    Program to quickly compute simulated AIRS radiances.


!CALL PROTOCOL
!    none (main program)


!INPUT PARAMETERS:
!    none


!OUTPUT PARAMETERS:
!    none


!INPUT/OUTPUT PARAMETERS:
!    none


!RETURN VALUES:
!    none


!PARENT(S)
!    none


!ROUTINES CALLED:
!    CALOWP : calc OPTRAN water predictors
!    CALPAR : calculate a profile's predictors
!    CALRAD : calc radiance
!    CALT1  : calc effective layer trans for set1 (FWO)
!    CALT2  : calc effective layer trans for set2 (FOW)
!    CALT3  : calc effective layer trans for set3 (FMW)
!    CALT4  : calc effective layer trans for set4 (FCOW)
!    CALT5  : calc effective layer trans for set5 (FWO bfsw)
!    CALT6  : calc effective layer trans for set6 (FWO mfmw)
!    CALT7  : calc effective layer trans for set7 (FWO mfbw)
!    FAKETZ : calc a "fake" (rough approx) surface-to-space trans
!    RDCOEF : read the fast transmittance coefficients
!    RDPROF : read profile data
!    RDSUN  : read the solar radiance datafile
!    SUNPAR : calc a profile's predictors for sets4-7 (sun channels)
!    CALNTE : calc radiance contribution for non-LTE


!FILES ACCESSED:
!    incFTC.f : include file of parameter statements accessed during
!       compilation only.
!    unit IOUN: used by routines RDCOEF and RDPROF%
!    unit 6: USEFAST text messages to the screen
!    unit 5: USEFAST user input instructions, etc
!    unit 10: USEFAST output radiance, text file(s)


!COMMON BLOCKS
!    COMLEV : layer boundary pressure levels


!DESCRIPTION:
!    May 2001 version of the SARTA_RTP (Stand-Alone Rapid
!    Transmittance Algorith with RTP I/O) by
!    L.L.Strow, S.Hannon, and H.Mottler

!    Computes radiances for the layers profiles contained in the
!    input RTP file.  This is the main program, and consists
!    primarily of calls to the external routines to do most of the
!    computing.


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    This program is only intended as a demo of the fast model.


!ROUTINE HISTORY:
! Date         Programmer    Comments
! ----------- -------------- -------------------------------------------
! 01 Dec 1994 Scott Hannon   Created
! 10 Apr 1995 Scott Hannon   New header comments; added ALT; new
!                            external function VACONV; SECANG may
!                            vary with layer
! 03 Jul 1995 Scott Hannon   Add parameter DZ/RDZ to RDPROF call
! 03 Feb 1997 Scott Hannon   Re-written for FWO+FOW+FMW+FCOW
! 12 Sep 1997 Scott Hannon   Re-written for 7 sets and reflected sun
!                            and downwelling thermal
! 30 Sep 1997 Scott Hannon   Added variable CO2
! 27 Feb 1998 Scott Hannon   Added OPTRAN water
! 26 Aug 1998 Scott Hannon   Added LBOT to calls to CALPAR, CALOWP,
!                            and SUNPAR; rename TBOT to TSURF; calc
!                            fractional bottom layer temperature and
!                            put it in TEMP(LBOT)
! 15 Oct 1999 Scott Hannon   Add ANGMAX and re-arrange angle conv
! 31 Mar 2000 Scott Hannon   Redid calpar for FIXMUL and added getbot
! 15 Mar 2001 Scott Hannon   Major re-write for RTP
! 03 May 2001 Scott Hannon   Add COMLEV; add PLEV to getbot call
! 13 Sep 2001 Scott Hannon   Changes to check of FCHAN vs FREQ
! 01 Nov 2002 Scott Hannon   Added SATZEN & SALT to RDRTP call, and
!                            if valid use SATZEN rather than SATANG
! 03 Jan 2003 Scott Hannon   Delete SUNSEC, add XZ & SUNFDG & code
!                            to fudge large sun angles (previously
!                            sunang>80 were treated as no sun).
! 24 Jul 2003 Scott Hannon   Fix error in TEMP(LBOT) calc for
!                            bottom fractional layer; add PLAY
! 06 Feb 2004 Scott Hannon   Add call to TUNMLT; add call to MEAN_T
!                            and associated prep code; add PTYPE
!                            to OPNRTP call; add LRHOT to RDINFO,
!                            OPNRTP, & SETEMS calls.
! 20 Dec 2004 Scott Hanonn   Add NLAY to getbot.f call; add PTYPE
!                            to rdrtp_so2.f call
! 18 May 2005 Scott Hannon   Add HNO3 based on SO2 code
! 28 Jun 2005 Scott Hannon   "trace" version for CO2,SO2,HNO3,N2O
! 13 Oct 2005 Scott Hannon   Add non-LTE
! 08 Dec 2005 Scott Hannon   Update tunmlt call for non-LTE tuning
! 02 May 2007 Scott Hannon   Replace hardcoded default SALT value
!                            with XSALT from incFTC.
! 23 Jan 2008 Scott Hannon   Add LCO2PM to allow CO2 profile in ppmv;
!                               add LCO2,LN2O,LSO2,LHNO3 switches
! 31 Jan 2008 Scott Hannon   Fix bug so LCO2,LN2O,LSO2,LHNO3 are LOGICAL
!                               as intended instead of REAL
! 05 May 2008 Scott Hannon   Set SUNCOS=0 when DOSUN=false
! 09 May 2008 Scott Hannon   Add freq adjustment code including A & B
!                            variables, two calls to rdcoef & tunmlt,
!                            new call to sumcof, and add yoff to rdrtp.
! 14 May 2008 Scott Hannon   Add default freq FRQDEF and WGTA,WGTB; add
!                               CO2TOP to calpar.f & calnte.f calls;
!                               add CO2PPM to calpar.f call; move no
!                               prof CO2MLT calc to calpar.f; add AORB
!                               to RDSUN
! 24 Oct 2008 Scott Hannon   Update for rtpV201
! 04 Aug 2009 Scott Hannon   Add call to SPECCAL; add DFCAL; add
!                               CLI<gas> for CO2,HNO,H2O,N2O,SO2 to
!                               RDCOEF & TUNMLT calls; add CLI<*> to
!                               SUMCOF call; adjust head.vchan
! 06 Aug 2009 Scott Hannon   Remove NCHAN and INDCHAN from SPECCAL call


!END====================================================================

!      =================================================================
PROGRAM SARTA
!      =================================================================


!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------
IMPLICIT NONE


!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
INCLUDE 'incFTC.f'
INCLUDE 'rtpdefs.f'


!-----------------------------------------------------------------------
!      EXTERNAL FUNCTIONS
!-----------------------------------------------------------------------
REAL :: VACONV
REAL :: SACONV


!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
!      none (main program)


!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------

INTEGER :: IOUN         ! I/O unit number

!      for RDINFO
CHARACTER (LEN=120) :: FIN       ! input RTP filename
CHARACTER (LEN=120) :: FOUT      ! output RTP filename
LOGICAL :: LRHOT         ! force refl therm rho=(1-emis)/pi?
INTEGER :: NWANTP         ! number of wanted profiles (-1=all)
INTEGER :: LISTP(MAXPRO) ! list of wanted profiles

!      for OPNRTP
INTEGER :: PTYPE         ! profile type
INTEGER :: NCHAN         ! # of selected channels
REAL :: FCHAN(MXCHAN) ! chan center frequency
INTEGER :: LSTCHN(MXCHAN) ! list of selected channels
INTEGER :: INDCHN(MXCHAN) ! array indices for all channels
INTEGER :: IH2O           ! index of H2O in gamnt
INTEGER :: IO3            ! index of O3 in gamnt
INTEGER :: ICO            ! index of CO in gamnt
INTEGER :: ICH4           ! index of CH4 in gamnt
INTEGER :: ICO2           ! index of CO2 in gamnt
INTEGER :: ISO2           ! index of SO2 in gamnt
INTEGER :: IHNO3          ! index of HNO3 in gamnt
INTEGER :: IN2O           ! index of N2O in gamnt
INTEGER :: IOPCI          ! input RTP unit
INTEGER :: IOPCO          ! output RTP unit
LOGICAL :: LCO2PM         ! CO2 profile in ppmv?

!      for RDCOEF             ! Info for selected channels only
CHARACTER (LEN=1) :: AORB     ! A or B
INTEGER :: SETCHN(MXCHAN) ! set # for each channel
INTEGER :: NCHN1         ! # of set1 channels
INTEGER :: NCHN2         ! # of set2 channels
INTEGER :: NCHN3         ! # of set3 channels
INTEGER :: NCHN4         ! # of set4 channels
INTEGER :: NCHN5         ! # of set5 channels
INTEGER :: NCHN6         ! # of set6 channels
INTEGER :: NCHN7         ! # of set7 channels
INTEGER :: CLIST1(MXCHN1) ! list of set1 channels
INTEGER :: CLIST2(MXCHN2) ! list of set2 channels
INTEGER :: CLIST3(MXCHN3) ! list of set3 channels
INTEGER :: CLIST4(MXCHN4) ! list of set4 channels
INTEGER :: CLIST5(MXCHN5) ! list of set5 channels
INTEGER :: CLIST6(MXCHN6) ! list of set6 channels
INTEGER :: CLIST7(MXCHN7) ! list of set7 channels
INTEGER :: CLICO2(MXCHNC) ! list of CO2 channels
INTEGER :: CLIHNO(MXCHNH) ! list of HNO3 channels
INTEGER :: CLIH2O(MXCHNW) ! list of H2O channels
INTEGER :: CLIN2O(MXCHNN) ! list of N2O channels
INTEGER :: CLISO2(MXCHNS) ! list of SO2 channels
INTEGER :: LABOVE(MXCHAN) ! chan downwelling thermal layer above
REAL :: FREQ(MXCHAN)    ! chan center frequency
REAL :: COEF1(N1COEF,MAXLAY,MXCHN1) ! coefs for set1 chans
REAL :: COEF2(N2COEF,MAXLAY,MXCHN2) ! coefs for set2 chans
REAL :: COEF3(N3COEF,MAXLAY,MXCHN3) ! coefs for set3 chans
REAL :: COEF4(N4COEF,MAXLAY,MXCHN4) ! coefs for set4 chans
REAL :: COEF5(N5COEF,MAXLAY,MXCHN5) ! coefs for set5 chans
REAL :: COEF6(N6COEF,MAXLAY,MXCHN6) ! coefs for set6 chans
REAL :: COEF7(N7COEF,MAXLAY,MXCHN7) ! coefs for set7 chans
REAL :: COEFF(NFCOEF,MXCHAN)        ! coefs for chan "F" factor
INTEGER :: NCHCO2                    ! number of CO2 chans
INTEGER :: NCHN2O                    ! number of N2O chans
INTEGER :: NCHSO2                    ! number of SO2 chans
INTEGER :: NCHHNO                    ! number of HNO3 chans
INTEGER :: INDCO2(MXCHAN)            ! chan indices for CO2 pert
INTEGER :: INDN2O(MXCHAN)            ! chan indices for N2O pert
INTEGER :: INDSO2(MXCHAN)            ! chan indices for SO2 pert
INTEGER :: INDHNO(MXCHAN)            ! chan indices for HNO3 pert
REAL :: COFCO2(  NCO2,MAXLAY,MXCHNC) ! coefs for CO2 pert
REAL :: COFN2O(  NN2O,MAXLAY,MXCHNN) ! coefs for N2O pert
REAL :: COFSO2(  NSO2,MAXLAY,MXCHNS) ! coefs for SO2 pert
REAL :: COFHNO( NHNO3,MAXLAY,MXCHNH) ! coefs for HNO3 pert
INTEGER :: NCHH2O                    ! number of OPTRAN H2O chans
INTEGER :: INDH2O(MXCHAN)            ! chan indices for OPTRAN H2O
REAL :: WAZOP(MXOWLY)              ! OPTRAN water l-to-s amounts
REAL :: WAVGOP(NOWAVG,MXOWLY)       ! OPTRAN raw predictor averages
REAL :: COFH2O(  NH2O,MXOWLY,MXCHNW) ! coefs for OPTRAN H2O
REAL :: FX(MAXLAY)               ! fixed gases adjustment
INTEGER :: NCHNTE                    ! number of non-LTE channels
INTEGER :: CLISTN(MXCNTE)            ! non-LTE channel list
REAL :: COEFN(NNCOEF,MXCNTE)        ! non-LTE coefficients

!      database "A"
REAL :: FREQA(MXCHAN)               ! chan center frequency
REAL :: HSUNA(MXCHAN)               ! solar irradiance
REAL :: COEFFA(NFCOEF,MXCHAN)        ! coefs for chan "F" factor
REAL :: COEFNA(NNCOEF,MXCNTE)        ! non-LTE coefficients
REAL :: COEF1A(N1COEF,MAXLAY,MXCHN1) ! coefs for set1 chans
REAL :: COEF2A(N2COEF,MAXLAY,MXCHN2) ! coefs for set2 chans
REAL :: COEF3A(N3COEF,MAXLAY,MXCHN3) ! coefs for set3 chans
REAL :: COEF4A(N4COEF,MAXLAY,MXCHN4) ! coefs for set4 chans
REAL :: COEF5A(N5COEF,MAXLAY,MXCHN5) ! coefs for set5 chans
REAL :: COEF6A(N6COEF,MAXLAY,MXCHN6) ! coefs for set6 chans
REAL :: COEF7A(N7COEF,MAXLAY,MXCHN7) ! coefs for set7 chans
REAL :: COFCOA(  NCO2,MAXLAY,MXCHNC) ! coefs for CO2 pert
REAL :: COFN2A(  NN2O,MAXLAY,MXCHNN) ! coefs for N2O pert
REAL :: COFSOA(  NSO2,MAXLAY,MXCHNS) ! coefs for SO2 pert
REAL :: COFHNA( NHNO3,MAXLAY,MXCHNH) ! coefs for HNO3 pert
REAL :: COFH2A(  NH2O,MXOWLY,MXCHNW) ! coefs for OPTRAN H2O

!      database "B"
REAL :: FREQB(MXCHAN)               ! chan center frequency
REAL :: HSUNB(MXCHAN)               ! solar irradiance
REAL :: COEFFB(NFCOEF,MXCHAN)        ! coefs for chan "F" factor
REAL :: COEFNB(NNCOEF,MXCNTE)        ! non-LTE coefficients
REAL :: COEF1B(N1COEF,MAXLAY,MXCHN1) ! coefs for set1 chans
REAL :: COEF2B(N2COEF,MAXLAY,MXCHN2) ! coefs for set2 chans
REAL :: COEF3B(N3COEF,MAXLAY,MXCHN3) ! coefs for set3 chans
REAL :: COEF4B(N4COEF,MAXLAY,MXCHN4) ! coefs for set4 chans
REAL :: COEF5B(N5COEF,MAXLAY,MXCHN5) ! coefs for set5 chans
REAL :: COEF6B(N6COEF,MAXLAY,MXCHN6) ! coefs for set6 chans
REAL :: COEF7B(N7COEF,MAXLAY,MXCHN7) ! coefs for set7 chans
REAL :: COFCOB(  NCO2,MAXLAY,MXCHNC) ! coefs for CO2 pert
REAL :: COFN2B(  NN2O,MAXLAY,MXCHNN) ! coefs for N2O pert
REAL :: COFSOB(  NSO2,MAXLAY,MXCHNS) ! coefs for SO2 pert
REAL :: COFHNB( NHNO3,MAXLAY,MXCHNH) ! coefs for HNO3 pert
REAL :: COFH2B(  NH2O,MXOWLY,MXCHNW) ! coefs for OPTRAN H2O

!      for SPECCAL
REAL :: DFCAL(MXCHAN)

!      for rtpopen
INTEGER :: rtpopen
CHARACTER (LEN=1) :: MODE

!      for FAKETZ
INTEGER :: NFAKE         ! # of channels to "fake"
INTEGER :: INDFAK(MXCHAN) ! indices of channels to fake

!      for RDPROF; reference profile
CHARACTER (LEN=40) :: RPNAM ! ref prof name/ID
REAL :: RALT(MAXLAY) ! ref prof layer altitude
REAL :: RDZ(MAXLAY) ! ref prof layer thickness
REAL :: RPRES(MAXLAY) ! ref prof layer average pressure
REAL :: RTEMP(MAXLAY) ! ref prof layer average temperature
REAL :: RFAMNT(MAXLAY) ! ref prof layer "fixed" (CO2) amount
REAL :: RWAMNT(MAXLAY) ! ref prof layer water (H2O) amount
REAL :: ROAMNT(MAXLAY) ! ref prof layer ozone (O3) amount
REAL :: RCAMNT(MAXLAY) ! ref prof layer carbon monoxide (CO) amount
REAL :: RMAMNT(MAXLAY) ! ref prof layer methane (CH4) amount
REAL :: RSAMNT(MAXLAY) ! ref prof layer sulfer dioxide (SO2) amount
REAL :: RHAMNT(MAXLAY) ! ref prof layer nitric acid (HNO3) amount
REAL :: RNAMNT(MAXLAY) ! ref prof layer nitrous oxide (N2O) amount

!      for RDRTP; profile to calculate
INTEGER :: NLAY        ! number of layers in profile
REAL :: LAT            ! prof latitude
REAL :: LON            ! prof longitude
REAL :: ALT(MAXLAY) ! prof layer altitudes
REAL :: TEMP(MAXLAY) ! prof layer average temperature
REAL :: WAMNT(MAXLAY) ! prof layer water (H2O) amount
REAL :: OAMNT(MAXLAY) ! prof layer ozone (O3) amount
REAL :: CAMNT(MAXLAY) ! prof layer carbon monoxide (CO) amount
REAL :: MAMNT(MAXLAY) ! prof layer methane (CH4) amount
REAL :: FAMNT(MAXLAY) ! prof layer CO2 amount
REAL :: SAMNT(MAXLAY) ! prof layer SO2 amount
REAL :: HAMNT(MAXLAY) ! prof layer HNO3 amount
REAL :: NAMNT(MAXLAY) ! prof layer N2O amount

!      for surface
INTEGER :: LBOT             ! bottom layer index number
INTEGER :: NEMIS             ! # of emis pts
REAL :: PSURF                ! surface pressure
REAL :: BLMULT                ! bottom layer fractional multiplier
REAL :: FEMIS(MXEMIS)        ! emis freq pts
REAL :: XEMIS(MXEMIS)        ! emis pts
REAL :: XRHO(MXEMIS)        ! reflec pts

!      for MEAN_T
REAL :: TPSEUD(MAXLAY)

!      for CALPAR
LOGICAL :: LCO2             ! CO2 profile switch
LOGICAL :: LN2O             ! N2O profile switch
LOGICAL :: LSO2             ! SO2 profile switch
LOGICAL :: LHNO3             ! HNO3 profile switch
REAL :: SECANG(MAXLAY)        ! local path angle secant
REAL :: FIXMUL(MAXLAY)        ! "fixed" amount multiplier (~1)
REAL :: CONPRD( N1CON,MAXLAY) ! water continuum predictors
REAL :: FPRED1( N1FIX,MAXLAY) ! set1 "fixed" predictors
REAL :: FPRED2( N2FIX,MAXLAY) ! set2 "fixed" predictors
REAL :: FPRED3( N3FIX,MAXLAY) ! set3 "fixed" predictors
REAL :: FPRED4( N4FIX,MAXLAY) ! set4 "fixed" predictors
REAL :: FPRED5( N5FIX,MAXLAY) ! set5 "fixed" predictors
REAL :: FPRED6( N6FIX,MAXLAY) ! set6 "fixed" predictors
REAL :: FPRED7( N7FIX,MAXLAY) ! set7 "fixed" predictors
REAL :: WPRED1( N1H2O,MAXLAY) ! set1 water predictors
REAL :: WPRED2( N2H2O,MAXLAY) ! set2 water predictors
REAL :: WPRED3( N3H2O,MAXLAY) ! set3 water predictors
REAL :: WPRED4( N4H2O,MAXLAY) ! set4 water predictors
REAL :: WPRED5( N5H2O,MAXLAY) ! set5 water predictors
REAL :: WPRED6( N6H2O,MAXLAY) ! set6 water predictors
REAL :: WPRED7( N7H2O,MAXLAY) ! set7 water predictors
REAL :: OPRED1(  N1O3,MAXLAY) ! set1 ozone predictors
REAL :: OPRED2(  N2O3,MAXLAY) ! set2 ozone predictors
REAL :: OPRED4(  N4O3,MAXLAY) ! set4 ozone predictors
REAL :: OPRED5(  N5O3,MAXLAY) ! set5 ozone predictors
REAL :: OPRED6(  N6O3,MAXLAY) ! set6 ozone predictors
REAL :: OPRED7(  N7O3,MAXLAY) ! set7 ozone predictors
REAL :: MPRED3( N3CH4,MAXLAY) ! set3 methane predictors
REAL :: CPRED4(  N4CO,MAXLAY) ! set4 carbon monoxide predictors
REAL :: TRCPRD(NTRACE,MAXLAY) ! trace gas pert perdictors
REAL :: CO2MLT(MAXLAY)        ! CO2 perturbation multiplier
REAL :: SO2MLT(MAXLAY)        ! SO2 perturbation multiplier
REAL :: HNOMLT(MAXLAY)        ! HNO3 perturbation multiplier
REAL :: N2OMLT(MAXLAY)        ! N2O perturbation multiplier
REAL :: CO2TOP                ! top layers CO2 mixing ratio

!      for CALOWP
REAL :: WAANG(MAXLAY)
INTEGER :: LOPMIN
INTEGER :: LOPMAX
REAL :: H2OPRD(  NH2O,MXOWLY)
LOGICAL :: LOPUSE(MXOWLY)
INTEGER :: LOPLOW(MAXLAY)
REAL :: DAOP(MAXLAY)

!      for CALT
REAL :: TAU(MAXLAY,MXCHAN) ! chan layer effective trans
REAL :: TAUZ(MXCHAN)        ! chan surface-to-space trans
REAL :: WAOP(MXOWLY)        ! OPTRAN abs coef scaling factor
REAL :: XZ                ! optical depth multiplier for TAUZ
LOGICAL :: LTAU             ! Calc all layer transmittances?

!      for CALRAD
REAL :: TSURF         ! surface temperature
REAL :: EMIS(MXCHAN) ! chan surface emissivity
REAL :: RHOSUN(MXCHAN) ! chan reflectivity for sun
REAL :: RHOTHR(MXCHAN) ! chan reflectivity for downwelling thermal
REAL :: RAD(MXCHAN) ! chan radiance
REAL :: BT(MXCHAN) ! chan brightness temperature

!      for RDSUN
REAL :: HSUN(MXCHAN) ! sun radiance (direct from sun)

!      Other variables for the sun
REAL :: SUNANG         ! solar zenith angle (at 0 altitude)
REAL :: SZALAY         ! solar zenith angle in some layer
REAL :: SUNCOS         ! cosine of sun zenith angle
REAL :: SCOS1          ! cosine of sun zenith angle at layer1
REAL :: SUNFDG         ! fudge factor for large solar angles
REAL :: SECSUN(MAXLAY) ! secant of effective sun local path angle
REAL :: DISTES         ! distance of Earth from the sun
REAL :: TAUZSN(MXCHAN) ! chan eff sun angle surface-to-space trans
LOGICAL :: DOSUN       ! do sun calc?

!      for satellite viewing angle
REAL :: SATANG      ! input satellite scan angle (degrees)
REAL :: SATZEN      ! input satellite zenith angle (degrees)
REAL :: SALT        ! input satellite altitude (kilometers)
REAL :: SVA         ! satellite viewing angle (degrees)

!      for RDRTP
INTEGER :: IPROF      ! profile loop counter
LOGICAL :: LWANT      ! do you want this profile?
REAL :: YOFF           ! yoffset used to calc grating model freqs

!      used locally only
INTEGER :: I      ! loop counter
INTEGER :: L      ! loop counter
INTEGER :: rtpclose    ! for call to RTP close interface routine
REAL :: EVA         ! (Earth) local viewing angle
REAL :: CONV         ! degrees to radians conversion factor
REAL :: ANGMAX         ! maximum allowed viewing angle
REAL :: FRQDEF         ! Default freq
REAL :: RJUNK1         ! junk/work
REAL :: RJUNK2         ! another junk/work
REAL :: CO2PPM         ! Profile mean dry air CO2 mixing ratio
REAL :: PLAY(MAXLAY)   ! layer mean pressure
REAL :: WGTA         ! weight "A"
REAL :: WGTB         ! weight "B"
REAL :: YOFPRE         ! previous YOFF

!      Profile data structure
INTEGER :: ISTAT
RECORD /RTPPROF/ PROF            ! profile
RECORD /RTPHEAD/ HEAD            ! header data
RECORD /RTPATTR/ HATT(MAXNATTR)  ! header attributes
RECORD /RTPATTR/ PATT(MAXNATTR)  ! profile attributes

!      Boundary pressure levels
COMMON /COMLEV/ PLEV
REAL :: PLEV(MAXLAY+1)

!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none

!***********************************************************************
!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!***********************************************************************

!      CONV = pi/180 = degrees to radians conversion factor
CONV=1.7453292E-02
YOFPRE=-999.0 ! dummy

!      --------------------------
!      Assign the I/O unit number
!      --------------------------
IOUN=11

!      ---------
!      Calc PLAY
!      ---------
!      Mean layer pressure (KLAYERS definition)
DO L=1,MAXLAY
  PLAY(L) = ( PLEV(L+1) - PLEV(L) )/LOG( PLEV(L+1)/PLEV(L) )
  ENDDO
    
    
!      -----------------------------
!      Read in the reference profile
!      -----------------------------
    CALL RDPROF(IOUN, FNPREF, RPNAM, RALT, RDZ, RPRES, RTEMP,  &
        RFAMNT, RWAMNT, ROAMNT, RCAMNT, RMAMNT, RSAMNT,RHAMNT,RNAMNT)
    
    
!      ---------------------
!      Get command-line info
!      ---------------------
    CALL RDINFO(FIN, FOUT, LRHOT, NWANTP, LISTP)
!cc
!      print *, 'nwantp=', NWANTP
!      print *, 'listp=', (LISTP(I),I=1,NWANTP)
!cc
    
!      ---------------------------
!      Open & check input RTP file
!      ---------------------------
    CALL OPNRTP(FIN, LRHOT, PTYPE, NCHAN, FCHAN, LSTCHN, INDCHN,  &
        IH2O, IO3, ICO, ICH4, ICO2, ISO2, IHNO3, IN2O,  &
        IOPCI, HEAD, HATT, PATT, LCO2PM)
    
    
!      ------------------------
!      Read the coef data files
!      ------------------------
!      Read the "A" coef database
    AORB='A'
    CALL RDCOEF ( AORB,  IOUN, NCHAN, INDCHN, SETCHN,  &
        NCHN1,  NCHN2,  NCHN3,  NCHN4,  NCHN5,  NCHN6,  NCHN7,  &
        CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7,  &
        COEF1A, COEF2A, COEF3A, COEF4A, COEF5A, COEF6A, COEF7A,  &
        NCHCO2, INDCO2, CLICO2, COFCOA, NCHN2O, INDN2O, CLIN2O, COFN2A,  &
        NCHSO2, INDSO2, CLISO2, COFSOA, NCHHNO, INDHNO, CLIHNO, COFHNA,  &
        NCHH2O, INDH2O, CLIH2O,  WAZOP, WAVGOP, COFH2A,  &
        FREQA, COEFFA, NCHNTE, CLISTN, COEFNA,     FX)
    
!      Get and apply multipler tuning to "A" coefficients
    CALL TUNMLT(AORB,    IOUN,  NCHAN, INDCHN, SETCHN,  &
        NCHN1,  NCHN2,  NCHN3,  NCHN4,  NCHN5,  NCHN6,  NCHN7,  &
        CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7,  &
        COEF1A, COEF2A, COEF3A, COEF4A, COEF5A, COEF6A, COEF7A,  &
        NCHCO2, INDCO2, CLICO2, COFCOA, NCHN2O, INDN2O, CLIN2O, COFN2A,  &
        NCHSO2, INDSO2, CLISO2, COFSOA, NCHHNO, INDHNO, CLIHNO, COFHNA,  &
        NCHH2O, INDH2O, CLIH2O,  WAZOP, WAVGOP, COFH2A,  &
        FREQA, COEFFA, NCHNTE, CLISTN, COEFNA,     FX)
    
!      Read the "B" coef database
    AORB='B'
    CALL RDCOEF (AORB,   IOUN, NCHAN, INDCHN, SETCHN,  &
        NCHN1,  NCHN2,  NCHN3,  NCHN4,  NCHN5,  NCHN6,  NCHN7,  &
        CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7,  &
        COEF1B, COEF2B, COEF3B, COEF4B, COEF5B, COEF6B, COEF7B,  &
        NCHCO2, INDCO2, CLICO2, COFCOB, NCHN2O, INDN2O, CLIN2O, COFN2B,  &
        NCHSO2, INDSO2, CLISO2, COFSOB, NCHHNO, INDHNO, CLIHNO, COFHNB,  &
        NCHH2O, INDH2O, CLIH2O,  WAZOP, WAVGOP, COFH2B,  &
        FREQB, COEFFB, NCHNTE, CLISTN, COEFNB,     FX)
    
!      Get and apply multipler tuning to "B" coefficients
    CALL TUNMLT( AORB,   IOUN,  NCHAN, INDCHN, SETCHN,  &
        NCHN1,  NCHN2,  NCHN3,  NCHN4,  NCHN5,  NCHN6,  NCHN7,  &
        CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7,  &
        COEF1B, COEF2B, COEF3B, COEF4B, COEF5B, COEF6B, COEF7B,  &
        NCHCO2, INDCO2, CLICO2, COFCOB, NCHN2O, INDN2O, CLIN2O, COFN2B,  &
        NCHSO2, INDSO2, CLISO2, COFSOB, NCHHNO, INDHNO, CLIHNO, COFHNB,  &
        NCHH2O, INDH2O, CLIH2O,  WAZOP, WAVGOP, COFH2B,  &
        FREQB, COEFFB, NCHNTE, CLISTN, COEFNB,     FX)
    
!      Calc OPTRAN absorption coefficient scaling factor WAOP
    WAOP(1)=WAZOP(1)
    DO L=2,MXOWLY
      WAOP(L)=WAZOP(L) - WAZOP(L-1)
      ENDDO
        
        
!      ---------------------------------
!      Read the spec cal adjustment file
!      ---------------------------------
        CALL SPECCAL(IOUN, DFCAL)
        
        
!      --------------------------
!      Read in the solar radiance
!      --------------------------
        AORB='A'
        CALL RDSUN(AORB, IOUN, INDCHN, HSUNA)
        AORB='B'
        CALL RDSUN(AORB, IOUN, INDCHN, HSUNB)
        
        DISTES=1.496E+11  ! distance Earth to Sun
        
        
!      --------------------
!      Check FREQ and FCHAN
!      --------------------
!      Note: FREQ comes the coef data, while FCHAN comes
!      from the input RTP file read by OPNRTP.  It is possible
!      that FCHAN is "nodata", so we check the first element.
        
        
        IF (FCHAN(1) > 640 .AND. FCHAN(1) < 2670) THEN
          DO I=1,NCHAN
!            Calc summing weights for default freq
            L=LSTCHN(I)
            WGTA=(YOFDEF + DFCAL(L) - YOFFB)/(YOFFA - YOFFB)
            WGTB=(YOFDEF + DFCAL(L) - YOFFA)/(YOFFB - YOFFA)
            
!            Check old channel freqs before updating
            FRQDEF=FREQA(I)*WGTA + FREQB(I)*WGTB
            RJUNK1=ABS(FRQDEF - FCHAN(I))
            RJUNK2=0.05*FRQDEF/1200.0   ! ~5% of a channel fullwidth
            IF (RJUNK1 > RJUNK2) THEN
              WRITE(IOINFO,1010) I, LSTCHN(I), FRQDEF, FCHAN(I)
              1010           FORMAT('Warning! index=',I4,', chan ID=',I4,  &
                  ', fastmodel default freq=',F8.3,', RTP freq=',F8.3)
            END IF
            HEAD%vchan(I)=FRQDEF
            ENDDO
            ELSE
!         Assign freqs without checking old freqs
              DO I=1,NCHAN
!            Calc summing weights for default freq
                L=LSTCHN(I)
                WGTA=(YOFDEF + DFCAL(L) - YOFFB)/(YOFFA - YOFFB)
                WGTB=(YOFDEF + DFCAL(L) - YOFFA)/(YOFFB - YOFFA)
                
                HEAD%vchan(I)=FREQA(I)*WGTA + FREQB(I)*WGTB
                ENDDO
                END IF
                
                
!      ------------------------
!      Open the output RTP file
!      ------------------------
                MODE='c'
                ISTAT=rtpopen(FOUT, MODE, HEAD, HATT, PATT, IOPCO)
!cc
!       print *, 'read open status = ', ISTAT
!cc
                
                
!      -----------------------------------------------
!      All channels from sets 1, 2, and 3 are to use a
!      fake effective sun angle layer-to-space trans
!      -----------------------------------------------
                NFAKE=0
                
                DO I=1,NCHN1
                  NFAKE=NFAKE + 1
                  INDFAK(NFAKE)=INDCHN( CLIST1(I) )
                  ENDDO
                    
                    DO I=1,NCHN2
                      NFAKE=NFAKE + 1
                      INDFAK(NFAKE)=INDCHN( CLIST2(I) )
                      ENDDO
                        
                        DO I=1,NCHN3
                          NFAKE=NFAKE + 1
                          INDFAK(NFAKE)=INDCHN( CLIST3(I) )
                          ENDDO
                            
                            
!      ---------------------------
!      Start of loop over profiles
!      ---------------------------
                            IPROF=1  ! initialize profile counter
!      Do you want this profile?
                            10    LWANT=.TRUE.
                            IF (NWANTP > 1) THEN
!         Look for current profile on list of wanted profiles
                              LWANT=.FALSE.
                              DO I=1,NWANTP
                                IF (IPROF == LISTP(I)) LWANT=.TRUE.
                                ENDDO
                                END IF
                                
                                
!      --------------
!      Read input RTP
!      --------------
                                CALL RDRTP( LWANT, IPROF, IOPCI,  &
                                    IH2O, IO3, ICO, ICH4, ICO2, ISO2, IHNO3, IN2O, PTYPE,  &
                                    RALT, LCO2PM, NLAY, NEMIS, LAT, LON, SATANG, SATZEN,  &
                                    SALT, SUNANG, PSURF, TSURF, CO2PPM, FEMIS, XEMIS, XRHO,  &
                                    TEMP, WAMNT, OAMNT, CAMNT, MAMNT, FAMNT, SAMNT, HAMNT, NAMNT,  &
                                    ALT, PROF, ISTAT, YOFF )
                                
                                IF (ISTAT == -1) GO TO 9999  ! reached End Of File
                                
                                IF (.NOT. LWANT) THEN
!         Skip this profile
                                  IPROF=IPROF+ 1
                                  GO TO 10
                                END IF
                                
                                
!      -------------------
!      Do freq adjustments
!      -------------------
                                IF (YOFF /= YOFPRE) THEN
                                  
!cc for testing
!      print *,'IPROF,YOFF,YOFPRE=',IPROF,YOFF,YOFPRE
!cc
                                  
                                  CALL SUMCOF( NCHAN, NCHNTE, NCHN1, NCHN2, NCHN3, NCHN4, NCHN5,  &
                                      NCHN6,  NCHN7, NCHCO2, NCHN2O, NCHSO2, NCHHNO, NCHH2O,  &
                                      LSTCHN, CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7,  &
                                      CLISTN, CLICO2, CLIN2O, CLISO2, CLIHNO, CLIH2O,   YOFF,  DFCAL,  &
                                      FREQA,  HSUNA, COEFFA, COEFNA, COEF1A, COEF2A, COEF3A, COEF4A,  &
                                      COEF5A, COEF6A, COEF7A, COFCOA, COFN2A, COFSOA, COFHNA, COFH2A,  &
                                      FREQB,  HSUNB, COEFFB, COEFNB, COEF1B, COEF2B, COEF3B, COEF4B,  &
                                      COEF5B, COEF6B, COEF7B, COFCOB, COFN2B, COFSOB, COFHNB, COFH2B,  &
                                      FREQ,   HSUN,  COEFF,  COEFN,  COEF1,  COEF2,  COEF3,  COEF4,  &
                                      COEF5,  COEF6,  COEF7, COFCO2, COFN2O, COFSO2, COFHNO, COFH2O)
                                  
!         Update YOFPRE for the newly summed coef data
                                  YOFPRE=YOFF
                                END IF
                                
                                
!      -------------------------------------
!      Determine bottom layer, CO2, & angles
!      -------------------------------------
                                CALL GETBOT(NLAY, PLEV, PSURF, LBOT, BLMULT)
!      Calc the fractional bottom layer air temperature
!cc
!       TEMP(LBOT)=TEMP(LBOT-1) + BLMULT*( TEMP(LBOT) - TEMP(LBOT-1) )
! Above line commented out & replaced by Scott Hannon, 24 July 2003.
! Mistakenly treats T at the center of the layer above as T at the
! bottom of the layer above.
!cc
                                
!      CO2 profile switch
                                IF (ICO2 < 1) THEN
                                  LCO2=.FALSE.
                                ELSE
                                  LCO2=.TRUE.
                                END IF
!      N2O profile switch
                                IF (IN2O < 1) THEN
                                  LN2O=.FALSE.
                                ELSE
                                  LN2O=.TRUE.
                                END IF
!      SO2 profile switch
                                IF (ISO2 < 1) THEN
                                  LSO2=.FALSE.
                                ELSE
                                  LSO2=.TRUE.
                                END IF
!      HNO3 profile switch
                                IF (IHNO3 < 1) THEN
                                  LHNO3=.FALSE.
                                ELSE
                                  LHNO3=.TRUE.
                                END IF
                                
                                
                                IF (PTYPE == AIRSLAY) THEN
!         Copy pseudo level temperatures to another array
                                  DO I=1,LBOT
                                    TPSEUD(I)=TEMP(I)
                                    ENDDO
!         Convert temperatures
                                      CALL MEAN_T(LBOT, PLEV, PSURF, TPSEUD, TEMP)
                                      
                                    ELSE
!         Calc mean pressure for bottom fractional layer
                                      RJUNK1 = ( PSURF - PLEV(LBOT) )/LOG( PSURF/PLEV(LBOT) )
!         Do interpolation for fractional bottom layer mean temperature
!         assuming T is in linear in log(P)
                                      RJUNK2=( TEMP(LBOT) - TEMP(LBOT-1) )/  &
                                          LOG( PLAY(LBOT)/PLAY(LBOT-1) )             ! slope
                                      TEMP(LBOT)=RJUNK2*LOG( RJUNK1/PLAY(LBOT-1) ) + TEMP(LBOT - 1)
                                    END IF
                                    
!      Check satellite elevation
                                    IF (SALT > 0) THEN
!         Warn and use default if invalid
                                      IF (SALT < XSALT-150 .OR. SALT > XSALT+150) THEN
!          IF (SALT .LT. XSALT-50 .OR. SALT .GT. XSALT+50) THEN
                                        WRITE(IOINFO,1020) IPROF, SALT, XSALT
                                        1020        FORMAT('Warning! Profile',I5,  &
                                            ': replacing invalid input satellite altitude ',  &
                                            1PE11.4,' with default ',1PE11.4,' km')
                                        SALT=XSALT
                                      END IF
                                    ELSE
                                      SALT=XSALT
                                    END IF
                                    
!      Convert SATZEN or SATANG to viewing angle
                                    IF (SATZEN >= 0 .AND. SATZEN < 63) THEN
!         Convert zenith angle at surface to view angle at satellite
                                      SVA=SACONV( SATZEN, SALT*1000 )/CONV
                                    ELSE
!         Check if scan angle is valid
                                      IF (SATANG > -49.6 .AND. SATANG < 49.6) THEN
!            View angle should be within a few degrees of scan angle
                                        SVA=ABS( SATANG )
                                      ELSE
                                        WRITE(IOERR,1030) IPROF, SATZEN, SATANG
                                        1030        FORMAT('Error! Profile',I5,  &
                                            ': invalid angles for SATZEN ',1PE11.4,  &
                                            ' and SATANG ',E11.4)
                                        STOP
                                      END IF
                                    END IF
                                    
                                    ANGMAX=53  ! max satellite view angle (49.5 scan + 3.5 spacecraft)
                                    IF (SVA > ANGMAX) THEN
!         Truncate angle if too big
                                      WRITE(IOINFO,1040) IPROF, SVA
                                      1040     FORMAT('Warning! Profile',I5,': truncating view angle ',  &
                                          1PE11.4,' to 53 degrees')
                                      SVA=ANGMAX
                                    END IF
                                    
!      Convert from satellite to earth viewing angle (in radians)
                                    DO L=1,LBOT
                                      EVA=VACONV(SVA, SALT, ALT(L))
                                      SECANG(L)=1.0E+0/COS(EVA)
!cccccccccccc
!            for testing
!             SECANG(L)=SVA
!cccccccccccc
                                      ENDDO
                                        
!      Calc total sun angle secant
                                        DOSUN=.FALSE.
                                        IF (SUNANG >= 0.0 .AND. SUNANG < 89.9) DOSUN=.TRUE.
                                        IF (DOSUN) THEN
                                          SUNCOS=COS(CONV*SUNANG)
                                          SZALAY=SACONV(SUNANG,ALT(1))
                                          SCOS1=COS(SZALAY)
                                          RJUNK2=SECANG(LBOT) + 1.0/SUNCOS ! Total secant
                                          
!         Calc non-unity fudge factor if total secant > 9
                                          IF (RJUNK2 > 9.0) THEN
!            fudge factor = true_total_secant/calc_total_secant
                                            SUNFDG=RJUNK2/9.0
!            truncated solar angle to use to calc SECSUN
                                            RJUNK1=ACOS( 1.0/(9.0 - SECANG(LBOT)) )/CONV
                                          ELSE
                                            SUNFDG=1.0
                                            RJUNK1=SUNANG
                                          END IF
                                          
                                          DO L=1,LBOT
                                            SZALAY=SACONV(RJUNK1,ALT(L))
                                            SECSUN(L)=SECANG(L) + 1.0E+0/COS(SZALAY)
                                            ENDDO
                                              
                                            END IF
                                            
                                            
!      -----------------------------------
!      Calculate the fast trans predictors
!      -----------------------------------
                                            
                                            CALL CALPAR (LBOT,  &
                                                RTEMP,RFAMNT,RWAMNT,ROAMNT,RCAMNT,RMAMNT,RSAMNT,RHAMNT,RNAMNT,  &
                                                TEMP, FAMNT, WAMNT, OAMNT, CAMNT, MAMNT, SAMNT, HAMNT, NAMNT,  &
                                                RPRES,SECANG,   LAT,    FX,   RDZ,  &
                                                LCO2,  LN2O,  LSO2, LHNO3,LCO2PM,CO2PPM,CO2TOP,FIXMUL,CONPRD,  &
                                                FPRED1,FPRED2,FPRED3,FPRED4,FPRED5,FPRED6,FPRED7,  &
                                                WPRED1,WPRED2,WPRED3,WPRED4,WPRED5,WPRED6,WPRED7,  &
                                                OPRED1,OPRED2,       OPRED4,OPRED5,OPRED6,OPRED7,  &
                                                MPRED3,CPRED4,TRCPRD,CO2MLT,SO2MLT,HNOMLT,N2OMLT )
                                            
                                            
!      -----------------------------------
!      Calculate the OPTRAN H2O predictors
!      -----------------------------------
                                            CALL CALOWP ( LBOT, WAMNT, RPRES, TEMP, SECANG, WAZOP, WAVGOP,  &
                                                WAANG, LOPMIN, LOPMAX, LOPUSE, H2OPRD, LOPLOW, DAOP )
                                            
                                            
!      ----------------------------------
!      Calculate the layer transmittances
!      ----------------------------------
!      Calculate TAU for set 1 thru 7
                                            
                                            CALL CALT1( INDCHN,  LBOT,  BLMULT,  NCHN1, CLIST1, COEF1,  &
                                                FIXMUL, CONPRD, FPRED1, WPRED1, OPRED1, TRCPRD,  &
                                                INDCO2, COFCO2, CO2MLT, INDSO2, COFSO2, SO2MLT,  &
                                                INDHNO, COFHNO, HNOMLT, INDN2O, COFN2O, N2OMLT,  &
                                                INDH2O, H2OPRD, COFH2O, LOPMIN, LOPMAX, LOPLOW,  &
                                                LOPUSE,   WAOP,   DAOP, WAANG,     TAU,   TAUZ)
                                            
                                            CALL CALT2( INDCHN, LBOT, BLMULT, NCHN2, CLIST2, COEF2,  &
                                                FIXMUL, CONPRD, FPRED2, OPRED2, WPRED2, TRCPRD,  &
                                                INDCO2, COFCO2, CO2MLT, INDSO2, COFSO2, SO2MLT,  &
                                                INDHNO, COFHNO, HNOMLT, INDN2O, COFN2O, N2OMLT, TAU, TAUZ)
                                            
                                            CALL CALT3( INDCHN,   LBOT, BLMULT,  NCHN3, CLIST3,  COEF3,  &
                                                FIXMUL, CONPRD, FPRED3, MPRED3, WPRED3, TRCPRD,  &
                                                INDSO2, COFSO2, SO2MLT, INDHNO, COFHNO, HNOMLT,  &
                                                INDN2O, COFN2O, N2OMLT,  &
                                                INDH2O, H2OPRD, COFH2O, LOPMIN, LOPMAX, LOPLOW, LOPUSE,  &
                                                WAOP,   DAOP,  WAANG,    TAU,   TAUZ)
                                            
                                            LTAU=.TRUE.
                                            XZ=1.0
                                            
                                            CALL CALT4(  LTAU, INDCHN,   LBOT, BLMULT,  NCHN4, CLIST4,  &
                                                COEF4, FIXMUL, CONPRD, FPRED4, CPRED4, OPRED4, WPRED4,  &
                                                TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT,  &
                                                XZ,    TAU,   TAUZ )
                                            
                                            CALL CALT5(  LTAU, INDCHN,   LBOT, BLMULT,  NCHN5, CLIST5,  &
                                                COEF5, FIXMUL, CONPRD, FPRED5, WPRED5, OPRED5,  &
                                                TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT,  &
                                                XZ,    TAU,   TAUZ )
                                            
                                            CALL CALT6(  LTAU, INDCHN,   LBOT, BLMULT,  NCHN6, CLIST6,  &
                                                COEF6, FIXMUL, CONPRD, FPRED6, WPRED6, OPRED6, TRCPRD,  &
                                                INDCO2, COFCO2, CO2MLT, INDSO2, COFSO2, SO2MLT,  &
                                                INDN2O, COFN2O, N2OMLT,     XZ,    TAU,   TAUZ )
                                            
                                            CALL CALT7(  LTAU, INDCHN,   LBOT, BLMULT,  NCHN7, CLIST7,  &
                                                COEF7, FIXMUL, CONPRD, FPRED7, WPRED7, OPRED7,  &
                                                TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT,  &
                                                XZ,    TAU,   TAUZ )
                                            
                                            IF (DOSUN) THEN
!         ---------------------------------------------
!         Calculate the fast trans predictors *for sun*
!         ---------------------------------------------
                                              
                                              CALL SUNPAR ( LBOT,  &
                                                  RTEMP, RWAMNT, ROAMNT, RCAMNT,  &
                                                  TEMP,  WAMNT,  OAMNT,  CAMNT,  &
                                                  RPRES,  SECSUN, CONPRD,  &
                                                  FPRED4, FPRED5, FPRED6, FPRED7,  &
                                                  WPRED4, WPRED5, WPRED6, WPRED7,  &
                                                  OPRED4, OPRED5, OPRED6, OPRED7,  &
                                                  CPRED4, TRCPRD )
                                              
!         --------------------------------------------
!         Calculate the layer transmittances *for sun*
!         --------------------------------------------
                                              
!         Calc fake TAUZSN for sets 1, 2, and 3
                                              RJUNK1=SUNFDG*SECSUN(LBOT)
                                              CALL FAKETZ( NFAKE, INDFAK, TAUZ, SECANG(LBOT),  &
                                                  RJUNK1, TAUZSN)
                                              
!         Calculate TAUZSN for sets 4 thru 7
                                              
                                              LTAU=.FALSE.
                                              XZ=SUNFDG
                                              
                                              CALL CALT4( LTAU, INDCHN, LBOT, BLMULT, NCHN4, CLIST4,  &
                                                  COEF4, FIXMUL, CONPRD, FPRED4, CPRED4, OPRED4, WPRED4,  &
                                                  TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT,  &
                                                  XZ, TAU, TAUZSN )
                                              
                                              CALL CALT5( LTAU, INDCHN, LBOT, BLMULT, NCHN5, CLIST5,  &
                                                  COEF5, FIXMUL, CONPRD, FPRED5, WPRED5, OPRED5,  &
                                                  TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT,  &
                                                  XZ, TAU, TAUZSN )
                                              
                                              CALL CALT6( LTAU, INDCHN, LBOT, BLMULT, NCHN6, CLIST6,  &
                                                  COEF6, FIXMUL, CONPRD, FPRED6, WPRED6, OPRED6,  &
                                                  TRCPRD, INDCO2, COFCO2, CO2MLT, INDSO2, COFSO2, SO2MLT,  &
                                                  INDN2O, COFN2O, N2OMLT, XZ, TAU, TAUZSN )
                                              
                                              CALL CALT7( LTAU, INDCHN, LBOT, BLMULT, NCHN7, CLIST7,  &
                                                  COEF7, FIXMUL, CONPRD, FPRED7, WPRED7, OPRED7,  &
                                                  TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT,  &
                                                  XZ, TAU, TAUZSN )
                                              
                                            ELSE
!         No sun; set the sun surface-to-space trans to zero
                                              SUNCOS=0.0
                                              DO I=1,NCHAN
                                                TAUZSN(I)=0.0
                                                ENDDO
                                                END IF
                                                
                                                
!      ---------------------------------------------------
!      Set the emissivity & reflectivity for every channel
!      ---------------------------------------------------
                                                CALL SETEMS( NCHAN, NEMIS, FREQ, FEMIS, XEMIS,  &
                                                    XRHO, LRHOT, EMIS, RHOSUN, RHOTHR)
                                                
                                                
!      ----------------------
!      Calculate the radiance
!      ----------------------
                                                CALL CALRAD ( NCHAN, FREQ, TAU, TEMP, TSURF, EMIS, LBOT,  &
                                                    SUNCOS, RHOSUN, DISTES, HSUN, TAUZSN,  &
                                                    SECANG(LBOT), RHOTHR, LABOVE, COEFF, TAUZ, RAD, BT)
                                                
!      -----------------
!      Calculate non-LTE
!      -----------------
                                                IF (DOSUN) THEN
                                                  CALL CALNTE ( INDCHN, TEMP, SUNCOS, SCOS1, SECANG(1),  &
                                                      NCHNTE, CLISTN, COEFN, CO2TOP, RAD )
                                                END IF
                                                
!cc
!      do I=1,NCHAN
!      print *, BT(I), RAD(I)
!      enddo
!cc
                                                
!      -------------------
!      Output the radiance
!      -------------------
                                                CALL WRTRTP(IPROF, IOPCO, NCHAN, RAD, PROF)
                                                
                                                
!      ----------------------
!      End loop over profiles
!      ----------------------
                                                IPROF=IPROF + 1  ! increment profile counter
                                                GO TO 10
                                                
                                                
!      -------------------
!      Close the RTP files
!      -------------------
                                                9999  ISTAT=rtpclose(IOPCI)
                                                ISTAT=rtpclose(IOPCO)
                                                
                                                STOP
                                              END PROGRAM SARTA
