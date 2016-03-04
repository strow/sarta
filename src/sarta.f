c This version with rtp input profiles and command-line arguments
C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore County (UMBC)
C
C    AIRS
C
C    SARTA version with trace gases
C
!F77====================================================================


!ROUTINE NAME:
C    SARTA


!ABSTRACT:
C    Program to quickly compute simulated AIRS radiances.


!CALL PROTOCOL
C    none (main program)


!INPUT PARAMETERS:
C    none


!OUTPUT PARAMETERS:
C    none


!INPUT/OUTPUT PARAMETERS:
C    none


!RETURN VALUES:
C    none


!PARENT(S)
C    none


!ROUTINES CALLED:
C    CALOWP : calc OPTRAN water predictors 
C    CALPAR : calculate a profile's predictors
C    CALRAD : calc radiance
C    CALT1  : calc effective layer trans for set1 (FWO)
C    CALT2  : calc effective layer trans for set2 (FOW)
C    CALT3  : calc effective layer trans for set3 (FMW)
C    CALT4  : calc effective layer trans for set4 (FCOW)
C    CALT5  : calc effective layer trans for set5 (FWO bfsw)
C    CALT6  : calc effective layer trans for set6 (FWO mfmw)
C    CALT7  : calc effective layer trans for set7 (FWO mfbw)
C    FAKETZ : calc a "fake" (rough approx) surface-to-space trans
C    RDCOEF : read the fast transmittance coefficients
C    RDPROF : read profile data
C    RDSUN  : read the solar radiance datafile
C    SUNPAR : calc a profile's predictors for sets4-7 (sun channels)
C    CALNTE : calc radiance contribution for non-LTE


!FILES ACCESSED:
C    incFTC.f : include file of parameter statements accessed during
C       compilation only.
C    unit IOUN: used by routines RDCOEF and RDPROF.
C    unit 6: USEFAST text messages to the screen
C    unit 5: USEFAST user input instructions, etc
C    unit 10: USEFAST output radiance, text file(s)


!COMMON BLOCKS
C    COMLEV : layer boundary pressure levels


!DESCRIPTION:
C    May 2001 version of the SARTA_RTP (Stand-Alone Rapid
C    Transmittance Algorith with RTP I/O) by
C    L.L.Strow, S.Hannon, and H.Mottler
C
C    Computes radiances for the layers profiles contained in the
C    input RTP file.  This is the main program, and consists
C    primarily of calls to the external routines to do most of the
C    computing.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    This program is only intended as a demo of the fast model.


!ROUTINE HISTORY:
C Date         Programmer    Comments
C ----------- -------------- -------------------------------------------
C 01 Dec 1994 Scott Hannon   Created
C 10 Apr 1995 Scott Hannon   New header comments; added ALT; new
C                            external function VACONV; SECANG may
C                            vary with layer
C 03 Jul 1995 Scott Hannon   Add parameter DZ/RDZ to RDPROF call
C 03 Feb 1997 Scott Hannon   Re-written for FWO+FOW+FMW+FCOW
C 12 Sep 1997 Scott Hannon   Re-written for 7 sets and reflected sun
C                            and downwelling thermal
C 30 Sep 1997 Scott Hannon   Added variable CO2
C 27 Feb 1998 Scott Hannon   Added OPTRAN water
C 26 Aug 1998 Scott Hannon   Added LBOT to calls to CALPAR, CALOWP,
C                            and SUNPAR; rename TBOT to TSURF; calc
C                            fractional bottom layer temperature and
C                            put it in TEMP(LBOT)
C 15 Oct 1999 Scott Hannon   Add ANGMAX and re-arrange angle conv
C 31 Mar 2000 Scott Hannon   Redid calpar for FIXMUL and added getbot
C 15 Mar 2001 Scott Hannon   Major re-write for RTP
C 03 May 2001 Scott Hannon   Add COMLEV; add PLEV to getbot call
C 13 Sep 2001 Scott Hannon   Changes to check of FCHAN vs FREQ
C 01 Nov 2002 Scott Hannon   Added SATZEN & SALT to RDRTP call, and
C                            if valid use SATZEN rather than SATANG
C 03 Jan 2003 Scott Hannon   Delete SUNSEC, add XZ & SUNFDG & code
C                            to fudge large sun angles (previously
C                            sunang>80 were treated as no sun).
C 24 Jul 2003 Scott Hannon   Fix error in TEMP(LBOT) calc for
C                            bottom fractional layer; add PLAY
C 06 Feb 2004 Scott Hannon   Add call to TUNMLT; add call to MEAN_T
C                            and associated prep code; add PTYPE
C                            to OPNRTP call; add LRHOT to RDINFO,
C                            OPNRTP, & SETEMS calls.
C 20 Dec 2004 Scott Hanonn   Add NLAY to getbot.f call; add PTYPE
C                            to rdrtp_so2.f call
C 18 May 2005 Scott Hannon   Add HNO3 based on SO2 code
C 28 Jun 2005 Scott Hannon   "trace" version for CO2,SO2,HNO3,N2O
C 13 Oct 2005 Scott Hannon   Add non-LTE
C 08 Dec 2005 Scott Hannon   Update tunmlt call for non-LTE tuning
C 02 May 2007 Scott Hannon   Replace hardcoded default SALT value
C                            with XSALT from incFTC.
C 23 Jan 2008 Scott Hannon   Add LCO2PM to allow CO2 profile in ppmv;
C                               add LCO2,LN2O,LSO2,LHNO3 switches
C 31 Jan 2008 Scott Hannon   Fix bug so LCO2,LN2O,LSO2,LHNO3 are LOGICAL
C                               as intended instead of REAL
C 05 May 2008 Scott Hannon   Set SUNCOS=0 when DOSUN=false
C 13 May 2008 Scott Hannon   Add CO2TOP to calpar.f & calnte.f calls;
C                               add CO2PPM to calpar.f call; move no
C                               prof CO2MLT calc to calpar.f
C 24 Oct 2008 Scott Hannon   Update for rtpV201 (remove NRHO, FRHO)

!END====================================================================

C      =================================================================
       PROGRAM SARTA
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
       REAL VACONV
       REAL SACONV


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      none (main program)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
C
       INTEGER   IOUN         ! I/O unit number
C
C      for RDINFO
       CHARACTER*80 FIN       ! input RTP filename
       CHARACTER*80 FOUT      ! output RTP filename
       LOGICAL  LRHOT         ! force refl therm rho=(1-emis)/pi?
       INTEGER NWANTP         ! number of wanted profiles (-1=all)
       INTEGER  LISTP(MAXPRO) ! list of wanted profiles
C
C      for OPNRTP
       INTEGER  PTYPE         ! profile type
       INTEGER  NCHAN         ! # of selected channels
       REAL     FCHAN(MXCHAN) ! chan center frequency
       INTEGER LSTCHN(MXCHAN) ! list of selected channels
       INTEGER INDCHN(MXCHAN) ! array indices for all channels
       INTEGER IH2O           ! index of H2O in gamnt
       INTEGER IO3            ! index of O3 in gamnt
       INTEGER ICO            ! index of CO in gamnt
       INTEGER ICH4           ! index of CH4 in gamnt
       INTEGER ICO2           ! index of CO2 in gamnt
       INTEGER ISO2           ! index of SO2 in gamnt
       INTEGER IHNO3          ! index of HNO3 in gamnt
       INTEGER IN2O           ! index of N2O in gamnt
       INTEGER IOPCI          ! input RTP unit
       INTEGER IOPCO          ! output RTP unit
       LOGICAL LCO2PM         ! CO2 profile in ppmv?
C
C      for RDCOEF             ! Info for selected channels only
       INTEGER SETCHN(MXCHAN) ! set # for each channel
       INTEGER  NCHN1         ! # of set1 channels
       INTEGER  NCHN2         ! # of set2 channels
       INTEGER  NCHN3         ! # of set3 channels
       INTEGER  NCHN4         ! # of set4 channels
       INTEGER  NCHN5         ! # of set5 channels
       INTEGER  NCHN6         ! # of set6 channels
       INTEGER  NCHN7         ! # of set7 channels
       INTEGER CLIST1(MXCHN1) ! list of set1 channels
       INTEGER CLIST2(MXCHN2) ! list of set2 channels
       INTEGER CLIST3(MXCHN3) ! list of set3 channels
       INTEGER CLIST4(MXCHN4) ! list of set4 channels
       INTEGER CLIST5(MXCHN5) ! list of set5 channels
       INTEGER CLIST6(MXCHN6) ! list of set6 channels
       INTEGER CLIST7(MXCHN7) ! list of set7 channels
       INTEGER LABOVE(MXCHAN) ! chan downwelling thermal layer above
       REAL   FREQ(MXCHAN)    ! chan center frequency
       REAL  COEF1(N1COEF,MAXLAY,MXCHN1) ! coefs for set1 chans
       REAL  COEF2(N2COEF,MAXLAY,MXCHN2) ! coefs for set2 chans
       REAL  COEF3(N3COEF,MAXLAY,MXCHN3) ! coefs for set3 chans
       REAL  COEF4(N4COEF,MAXLAY,MXCHN4) ! coefs for set4 chans
       REAL  COEF5(N5COEF,MAXLAY,MXCHN5) ! coefs for set5 chans
       REAL  COEF6(N6COEF,MAXLAY,MXCHN6) ! coefs for set6 chans
       REAL  COEF7(N7COEF,MAXLAY,MXCHN7) ! coefs for set7 chans
       REAL  COEFF(NFCOEF,MXCHAN)        ! coefs for chan "F" factor
       INTEGER INDCO2(MXCHAN)            ! chan indices for CO2 pert
       REAL COFCO2(  NCO2,MAXLAY,MXCHNC) ! coefs for CO2 pert
       INTEGER INDSO2(MXCHAN)            ! chan indices for SO2 pert
       REAL COFSO2(  NSO2,MAXLAY,MXCHNS) ! coefs for SO2 pert
       INTEGER INDHNO(MXCHAN)            ! chan indices for HNO3 pert
       REAL COFHNO( NHNO3,MAXLAY,MXCHNH) ! coefs for HNO3 pert
       INTEGER INDN2O(MXCHAN)            ! chan indices for N2O pert
       REAL COFN2O(  NN2O,MAXLAY,MXCHNN) ! coefs for N2O pert
       INTEGER INDH2O(MXCHAN)            ! chan indices for OPTRAN H2O
       REAL   WAZOP(MXOWLY)              ! OPTRAN water l-to-s amounts
       REAL  WAVGOP(NOWAVG,MXOWLY)       ! OPTRAN raw predictor averages
       REAL COFH2O(  NH2O,MXOWLY,MXCHNW) ! coefs for OPTRAN H2O
       REAL     FX(MAXLAY)               ! fixed gases adjustment
       INTEGER NCHNTE                    ! number of non-LTE channels
       INTEGER CLISTN(MXCNTE)            ! non-LTE channel list
       REAL  COEFN(NNCOEF,MXCNTE)        ! non-LTE coefficients
C
C      for rtpopen
       INTEGER rtpopen
       CHARACTER*1 MODE
C
C      for FAKETZ
       INTEGER  NFAKE         ! # of channels to "fake"
       INTEGER INDFAK(MXCHAN) ! indices of channels to fake
C
C      for RDPROF; reference profile
       CHARACTER*40  RPNAM ! ref prof name/ID
       REAL   RALT(MAXLAY) ! ref prof layer altitude
       REAL    RDZ(MAXLAY) ! ref prof layer thickness
       REAL  RPRES(MAXLAY) ! ref prof layer average pressure
       REAL  RTEMP(MAXLAY) ! ref prof layer average temperature
       REAL RFAMNT(MAXLAY) ! ref prof layer "fixed" (CO2) amount
       REAL RWAMNT(MAXLAY) ! ref prof layer water (H2O) amount
       REAL ROAMNT(MAXLAY) ! ref prof layer ozone (O3) amount
       REAL RCAMNT(MAXLAY) ! ref prof layer carbon monoxide (CO) amount
       REAL RMAMNT(MAXLAY) ! ref prof layer methane (CH4) amount
       REAL RSAMNT(MAXLAY) ! ref prof layer sulfer dioxide (SO2) amount
       REAL RHAMNT(MAXLAY) ! ref prof layer nitric acid (HNO3) amount
       REAL RNAMNT(MAXLAY) ! ref prof layer nitrous oxide (N2O) amount
C
C      for RDRTP; profile to calculate
       INTEGER NLAY        ! number of layers in profile
       REAL LAT            ! prof latitude
       REAL LON            ! prof longitude
       REAL    ALT(MAXLAY) ! prof layer altitudes
       REAL   TEMP(MAXLAY) ! prof layer average temperature
       REAL  WAMNT(MAXLAY) ! prof layer water (H2O) amount
       REAL  OAMNT(MAXLAY) ! prof layer ozone (O3) amount
       REAL  CAMNT(MAXLAY) ! prof layer carbon monoxide (CO) amount
       REAL  MAMNT(MAXLAY) ! prof layer methane (CH4) amount
       REAL  FAMNT(MAXLAY) ! prof layer CO2 amount
       REAL  SAMNT(MAXLAY) ! prof layer SO2 amount
       REAL  HAMNT(MAXLAY) ! prof layer HNO3 amount
       REAL  NAMNT(MAXLAY) ! prof layer N2O amount
C
C      for surface
       INTEGER   LBOT             ! bottom layer index number
       INTEGER  NEMIS             ! # of emis pts
       REAL  PSURF                ! surface pressure
       REAL BLMULT                ! bottom layer fractional multiplier
       REAL  FEMIS(MXEMIS)        ! emis freq pts
       REAL  XEMIS(MXEMIS)        ! emis pts
       REAL   XRHO(MXEMIS)        ! reflec pts
C
C      for MEAN_T
       REAL TPSEUD(MAXLAY)
C
C      for CALPAR
       LOGICAL   LCO2             ! CO2 profile switch
       LOGICAL   LN2O             ! N2O profile switch
       LOGICAL   LSO2             ! SO2 profile switch
       LOGICAL  LHNO3             ! HNO3 profile switch
       REAL SECANG(MAXLAY)        ! local path angle secant
       REAL FIXMUL(MAXLAY)        ! "fixed" amount multiplier (~1)
       REAL CONPRD( N1CON,MAXLAY) ! water continuum predictors
       REAL FPRED1( N1FIX,MAXLAY) ! set1 "fixed" predictors
       REAL FPRED2( N2FIX,MAXLAY) ! set2 "fixed" predictors
       REAL FPRED3( N3FIX,MAXLAY) ! set3 "fixed" predictors
       REAL FPRED4( N4FIX,MAXLAY) ! set4 "fixed" predictors
       REAL FPRED5( N5FIX,MAXLAY) ! set5 "fixed" predictors
       REAL FPRED6( N6FIX,MAXLAY) ! set6 "fixed" predictors
       REAL FPRED7( N7FIX,MAXLAY) ! set7 "fixed" predictors
       REAL WPRED1( N1H2O,MAXLAY) ! set1 water predictors
       REAL WPRED2( N2H2O,MAXLAY) ! set2 water predictors
       REAL WPRED3( N3H2O,MAXLAY) ! set3 water predictors
       REAL WPRED4( N4H2O,MAXLAY) ! set4 water predictors
       REAL WPRED5( N5H2O,MAXLAY) ! set5 water predictors
       REAL WPRED6( N6H2O,MAXLAY) ! set6 water predictors
       REAL WPRED7( N7H2O,MAXLAY) ! set7 water predictors
       REAL OPRED1(  N1O3,MAXLAY) ! set1 ozone predictors
       REAL OPRED2(  N2O3,MAXLAY) ! set2 ozone predictors
       REAL OPRED4(  N4O3,MAXLAY) ! set4 ozone predictors
       REAL OPRED5(  N5O3,MAXLAY) ! set5 ozone predictors
       REAL OPRED6(  N6O3,MAXLAY) ! set6 ozone predictors
       REAL OPRED7(  N7O3,MAXLAY) ! set7 ozone predictors
       REAL MPRED3( N3CH4,MAXLAY) ! set3 methane predictors
       REAL CPRED4(  N4CO,MAXLAY) ! set4 carbon monoxide predictors
       REAL TRCPRD(NTRACE,MAXLAY) ! trace gas pert perdictors
       REAL CO2MLT(MAXLAY)        ! CO2 perturbation multiplier
       REAL SO2MLT(MAXLAY)        ! SO2 perturbation multiplier
       REAL HNOMLT(MAXLAY)        ! HNO3 perturbation multiplier
       REAL N2OMLT(MAXLAY)        ! N2O perturbation multiplier
       REAL CO2TOP                ! top layers CO2 mixing ratio 
C
C      for CALOWP
       REAL  WAANG(MAXLAY)
       INTEGER LOPMIN
       INTEGER LOPMAX
       REAL H2OPRD(  NH2O,MXOWLY)
       LOGICAL LOPUSE(MXOWLY)
       INTEGER LOPLOW(MAXLAY)
       REAL  DAOP(MAXLAY)
C
C      for CALT
       REAL    TAU(MAXLAY,MXCHAN) ! chan layer effective trans
       REAL   TAUZ(MXCHAN)        ! chan surface-to-space trans
       REAL   WAOP(MXOWLY)        ! OPTRAN abs coef scaling factor
       REAL     XZ                ! optical depth multiplier for TAUZ
       LOGICAL   LTAU             ! Calc all layer transmittances?
C
C      for CALRAD
       REAL  TSURF         ! surface temperature
       REAL   EMIS(MXCHAN) ! chan surface emissivity
       REAL RHOSUN(MXCHAN) ! chan reflectivity for sun
       REAL RHOTHR(MXCHAN) ! chan reflectivity for downwelling thermal
       REAL    RAD(MXCHAN) ! chan radiance
       REAL     BT(MXCHAN) ! chan brightness temperature
C
C      for RDSUN
       REAL   HSUN(MXCHAN) ! sun radiance (direct from sun)
C
C      Other variables for the sun
       REAL SUNANG         ! solar zenith angle (at 0 altitude)
       REAL SZALAY         ! solar zenith angle in some layer
       REAL SUNCOS         ! cosine of sun zenith angle
       REAL SCOS1          ! cosine of sun zenith angle at layer1
       REAL SUNFDG         ! fudge factor for large solar angles
       REAL SECSUN(MAXLAY) ! secant of effective sun local path angle
       REAL DISTES         ! distance of Earth from the sun
       REAL TAUZSN(MXCHAN) ! chan eff sun angle surface-to-space trans
       LOGICAL DOSUN       ! do sun calc?
C
C      for satellite viewing angle
       REAL    SATANG      ! input satellite scan angle (degrees)
       REAL    SATZEN      ! input satellite zenith angle (degrees)
       REAL    SALT        ! input satellite altitude (kilometers)
       REAL    SVA         ! satellite viewing angle (degrees)
C
C      for RDRTP
       INTEGER  IPROF      ! profile loop counter
       LOGICAL  LWANT      ! do you want this profile?

C      used locally only
       INTEGER      I      ! loop counter
       INTEGER      L      ! loop counter
       INTEGER rtpclose    ! for call to RTP close interface routine
       REAL    EVA         ! (Earth) local viewing angle
       REAL   CONV         ! degrees to radians conversion factor
       REAL ANGMAX         ! maximum allowed viewing angle
       REAL RJUNK1         ! junk/work
       REAL RJUNK2         ! another junk/work
       REAL CO2PPM         ! Profile mean dry air CO2 mixing ratio
       REAL PLAY(MAXLAY)   ! layer mean pressure
C
C      Profile data structure
       INTEGER  ISTAT
       RECORD /RTPPROF/ PROF            ! profile
       RECORD /RTPHEAD/ HEAD            ! header data
       RECORD /RTPATTR/ HATT(MAXNATTR)  ! header attributes
       RECORD /RTPATTR/ PATT(MAXNATTR)  ! profile attributes
C
C      Boundary pressure levels
       COMMON /COMLEV/ PLEV
       REAL PLEV(MAXLAY+1)

C-----------------------------------------------------------------------
C      SAVE STATEMENTS
C-----------------------------------------------------------------------
C      none
C
C***********************************************************************
C***********************************************************************
C                    EXECUTABLE CODE
C***********************************************************************
C***********************************************************************
C
C      CONV = pi/180 = degrees to radians conversion factor
       CONV=1.7453292E-02
C
C      --------------------------
C      Assign the I/O unit number
C      --------------------------
       IOUN=11
C
C      ---------
C      Calc PLAY
C      ---------
C      Mean layer pressure (KLAYERS definition)
       DO L=1,MAXLAY
          PLAY(L) = ( PLEV(L+1) - PLEV(L) )/LOG( PLEV(L+1)/PLEV(L) )
       ENDDO


C      -----------------------------
C      Read in the reference profile
C      -----------------------------
       CALL RDPROF(IOUN, FNPREF, RPNAM, RALT, RDZ, RPRES, RTEMP,
     $    RFAMNT, RWAMNT, ROAMNT, RCAMNT, RMAMNT, RSAMNT,RHAMNT,RNAMNT)
C

C      ---------------------
C      Get command-line info
C      ---------------------
       CALL RDINFO(FIN, FOUT, LRHOT, NWANTP, LISTP)
ccc
c      print *, 'nwantp=', NWANTP
c      print *, 'listp=', (LISTP(I),I=1,NWANTP)
ccc
C
C      ---------------------------
C      Open & check input RTP file
C      ---------------------------
       CALL OPNRTP(FIN, LRHOT, PTYPE, NCHAN, FCHAN, LSTCHN, INDCHN,
     $    IH2O, IO3, ICO, ICH4, ICO2, ISO2, IHNO3, IN2O,
     $    IOPCI, HEAD, HATT, PATT, LCO2PM)
C

C      ------------------------
C      Read the coef data files
C      ------------------------
       CALL RDCOEF( IOUN, NCHAN, INDCHN, SETCHN,
     $  NCHN1,  NCHN2,  NCHN3,  NCHN4,  NCHN5,  NCHN6,  NCHN7,
     $ CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7,
     $  COEF1,  COEF2,  COEF3,  COEF4,  COEF5,  COEF6,  COEF7,
     $   FREQ, LABOVE,  COEFF, INDCO2, COFCO2, INDSO2, COFSO2,
     $ INDHNO, COFHNO, INDN2O, COFN2O,
     $ INDH2O,  WAZOP, WAVGOP, COFH2O, FX, NCHNTE, CLISTN, COEFN )
C
C      Get and apply multipler tuning to coefficients {note: ignores HNO3}
       CALL TUNMLT( IOUN, NCHAN, INDCHN, SETCHN,
     $  NCHN1,  NCHN2,  NCHN3,  NCHN4,  NCHN5,  NCHN6,  NCHN7,
     $ CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7,
     $  COEF1,  COEF2,  COEF3,  COEF4,  COEF5,  COEF6,  COEF7,
     $   FREQ, LABOVE,  COEFF, INDCO2, COFCO2, INDSO2, COFSO2,
     $ INDHNO, COFHNO, INDN2O, COFN2O,
     $ INDH2O,  WAZOP, WAVGOP, COFH2O, FX, NCHNTE, CLISTN, COEFN )
C
C      Calc OPTRAN absorption coefficient scaling factor WAOP
       WAOP(1)=WAZOP(1)
       DO L=2,MXOWLY
          WAOP(L)=WAZOP(L) - WAZOP(L-1)
       ENDDO
C

C      --------------------------
C      Read in the solar radiance
C      --------------------------
       CALL RDSUN(IOUN, INDCHN, HSUN)
C
       DISTES=1.496E+11  ! distance Earth to Sun
C

C      --------------------
C      Check FREQ and FCHAN
C      --------------------
C      Note: FREQ comes the coef data, while FCHAN comes
C      from the input RTP file read by OPNRTP.  It is possible
C      that FCHAN is "nodata", so we check the first element.
       IF (FCHAN(1) .GT. 640 .AND. FCHAN(1) .LT. 2670) THEN
          DO I=1,NCHAN
             RJUNK1=ABS(FREQ(I) - FCHAN(I))
             RJUNK2=0.01*FREQ(I)/1200.0   ! ~1% of a channel fullwidth
             IF (RJUNK1 .GT. RJUNK2) THEN
                WRITE(IOINFO,1010) I, LSTCHN(I), FREQ(I), FCHAN(I)
 1010           FORMAT('Warning! index=',I4,', chan ID=',I4,
     $          ', fastmodel freq=',F8.3,', RTP freq=',F8.3)
             ENDIF
             HEAD.vchan(I)=FREQ(I)
          ENDDO
       ELSE
          DO I=1,NCHAN
             HEAD.vchan(I)=FREQ(I)
          ENDDO
       ENDIF
C

C      ------------------------
C      Open the output RTP file
C      ------------------------
       MODE='c'
       ISTAT=rtpopen(FOUT, MODE, HEAD, HATT, PATT, IOPCO)
ccc
c       print *, 'read open status = ', ISTAT
ccc
C

C      -----------------------------------------------
C      All channels from sets 1, 2, and 3 are to use a
C      fake effective sun angle layer-to-space trans
C      -----------------------------------------------
       NFAKE=0
C 
       DO I=1,NCHN1
          NFAKE=NFAKE + 1
          INDFAK(NFAKE)=INDCHN( CLIST1(I) )
       ENDDO
C
       DO I=1,NCHN2
          NFAKE=NFAKE + 1
          INDFAK(NFAKE)=INDCHN( CLIST2(I) )
       ENDDO
C
       DO I=1,NCHN3
          NFAKE=NFAKE + 1
          INDFAK(NFAKE)=INDCHN( CLIST3(I) )
       ENDDO
C

C      ---------------------------
C      Start of loop over profiles
C      ---------------------------
       IPROF=1  ! initialize profile counter
C      Do you want this profile?
 10    LWANT=.TRUE.
       IF (NWANTP .GT. 1) THEN
C         Look for current profile on list of wanted profiles
          LWANT=.FALSE.
          DO I=1,NWANTP
             IF (IPROF .EQ. LISTP(I)) LWANT=.TRUE.
          ENDDO
       ENDIF
C

C      --------------
C      Read input RTP
C      --------------
       CALL RDRTP( LWANT, IPROF, IOPCI,
     $    IH2O, IO3, ICO, ICH4, ICO2, ISO2, IHNO3, IN2O, PTYPE,
     $    RALT, LCO2PM, NLAY, NEMIS, LAT, LON, SATANG, SATZEN,
     $    SALT, SUNANG, PSURF, TSURF, CO2PPM, FEMIS, XEMIS, XRHO,
     $    TEMP, WAMNT, OAMNT, CAMNT, MAMNT, FAMNT, SAMNT, HAMNT, NAMNT,
     $     ALT, PROF, ISTAT )
C
       IF (ISTAT .EQ. -1) GOTO 9999  ! reached End Of File
C
       IF (.NOT. LWANT) THEN
C         Skip this profile
          IPROF=IPROF+ 1 
          GOTO 10
       ENDIF
C

C      -------------------------------------
C      Determine bottom layer, CO2, & angles
C      -------------------------------------
       CALL GETBOT(NLAY, PLEV, PSURF, LBOT, BLMULT)
C      Calc the fractional bottom layer air temperature
ccc
c       TEMP(LBOT)=TEMP(LBOT-1) + BLMULT*( TEMP(LBOT) - TEMP(LBOT-1) )
c Above line commented out & replaced by Scott Hannon, 24 July 2003.
c Mistakenly treats T at the center of the layer above as T at the
c bottom of the layer above.
ccc

C      CO2 profile switch
       IF (ICO2 .LT. 1) THEN
          LCO2=.FALSE.
       ELSE
          LCO2=.TRUE.
       ENDIF
C      N2O profile switch
       IF (IN2O .LT. 1) THEN
          LN2O=.FALSE.
       ELSE
          LN2O=.TRUE.
       ENDIF
C      SO2 profile switch
       IF (ISO2 .LT. 1) THEN
          LSO2=.FALSE.
       ELSE
          LSO2=.TRUE.
       ENDIF
C      HNO3 profile switch
       IF (IHNO3 .LT. 1) THEN
          LHNO3=.FALSE.
       ELSE
          LHNO3=.TRUE.
       ENDIF
C
C
       IF (PTYPE .EQ. AIRSLAY) THEN
C         Copy pseudo level temperatures to another array
          DO I=1,LBOT
             TPSEUD(I)=TEMP(I)
          ENDDO
C         Convert temperatures
          CALL MEAN_T(LBOT, PLEV, PSURF, TPSEUD, TEMP)
C
       ELSE
C         Calc mean pressure for bottom fractional layer
          RJUNK1 = ( PSURF - PLEV(LBOT) )/LOG( PSURF/PLEV(LBOT) )
C         Do interpolation for fractional bottom layer mean temperature
C         assuming T is in linear in log(P)
          RJUNK2=( TEMP(LBOT) - TEMP(LBOT-1) )/
     $       LOG( PLAY(LBOT)/PLAY(LBOT-1) )             ! slope
          TEMP(LBOT)=RJUNK2*LOG( RJUNK1/PLAY(LBOT-1) ) + TEMP(LBOT - 1)
       ENDIF
C
C      Check satellite elevation
       IF (SALT .GT. 0) THEN
C         Warn and use default if invalid
          IF (SALT .LT. XSALT-150 .OR. SALT .GT. XSALT+150) THEN
c          IF (SALT .LT. XSALT-50 .OR. SALT .GT. XSALT+50) THEN
             WRITE(IOINFO,1020) IPROF, SALT, XSALT
 1020        FORMAT('Warning! Profile',I5,
     $          ': replacing invalid input satellite altitude ',
     $          1PE11.4,' with default ',1PE11.4,' km')
             SALT=XSALT
          ENDIF
       ELSE
          SALT=XSALT
       ENDIF
C
C      Convert SATZEN or SATANG to viewing angle
       IF (SATZEN .GE. 0 .AND. SATZEN .LT. 63) THEN
C         Convert zenith angle at surface to view angle at satellite
          SVA=SACONV( SATZEN, SALT*1000 )/CONV
       ELSE
C         Check if scan angle is valid
          IF (SATANG .GT. -49.6 .AND. SATANG .LT. 49.6) THEN
C            View angle should be within a few degrees of scan angle
             SVA=ABS( SATANG )
          ELSE
             WRITE(IOERR,1030) IPROF, SATZEN, SATANG
 1030        FORMAT('Error! Profile',I5,
     $          ': invalid angles for SATZEN ',1PE11.4,
     $          ' and SATANG ',E11.4) 
             STOP
          ENDIF
       ENDIF

       ANGMAX=53  ! max satellite view angle (49.5 scan + 3.5 spacecraft)
       IF (SVA .GT. ANGMAX) THEN
C         Truncate angle if too big
          WRITE(IOINFO,1040) IPROF, SVA
 1040     FORMAT('Warning! Profile',I5,': truncating view angle ',
     $       1PE11.4,' to 53 degrees')
          SVA=ANGMAX
       ENDIF

C      Convert from satellite to earth viewing angle (in radians)
       DO L=1,LBOT
             EVA=VACONV(SVA, SALT, ALT(L))
             SECANG(L)=1.0E+0/COS(EVA)
ccccccccccccc
c            for testing
c             SECANG(L)=SVA
ccccccccccccc
       ENDDO
C
C      Calc total sun angle secant
       DOSUN=.FALSE.
       IF (SUNANG .GE. 0.0 .AND. SUNANG .LT. 89.9) DOSUN=.TRUE.
       IF (DOSUN) THEN
          SUNCOS=COS(CONV*SUNANG)
          SZALAY=SACONV(SUNANG,ALT(1))
          SCOS1=COS(SZALAY)
          RJUNK2=SECANG(LBOT) + 1.0/SUNCOS ! Total secant
C
C         Calc non-unity fudge factor if total secant > 9
          IF (RJUNK2 .GT. 9.0) THEN
C            fudge factor = true_total_secant/calc_total_secant
             SUNFDG=RJUNK2/9.0
C            truncated solar angle to use to calc SECSUN
             RJUNK1=ACOS( 1.0/(9.0 - SECANG(LBOT)) )/CONV
          ELSE
             SUNFDG=1.0
             RJUNK1=SUNANG
          ENDIF
C
          DO L=1,LBOT
             SZALAY=SACONV(RJUNK1,ALT(L))
             SECSUN(L)=SECANG(L) + 1.0E+0/COS(SZALAY)
          ENDDO

       ENDIF
C

C      -----------------------------------
C      Calculate the fast trans predictors
C      -----------------------------------
C
       CALL CALPAR (LBOT,
     $    RTEMP,RFAMNT,RWAMNT,ROAMNT,RCAMNT,RMAMNT,RSAMNT,RHAMNT,RNAMNT,
     $     TEMP, FAMNT, WAMNT, OAMNT, CAMNT, MAMNT, SAMNT, HAMNT, NAMNT,
     $    RPRES,SECANG,   LAT,    FX,   RDZ,
     $     LCO2,  LN2O,  LSO2, LHNO3,LCO2PM,CO2PPM,CO2TOP,FIXMUL,CONPRD,
     $   FPRED1,FPRED2,FPRED3,FPRED4,FPRED5,FPRED6,FPRED7,
     $   WPRED1,WPRED2,WPRED3,WPRED4,WPRED5,WPRED6,WPRED7,
     $   OPRED1,OPRED2,       OPRED4,OPRED5,OPRED6,OPRED7,
     $   MPRED3,CPRED4,TRCPRD,CO2MLT,SO2MLT,HNOMLT,N2OMLT )
C

C      -----------------------------------
C      Calculate the OPTRAN H2O predictors
C      -----------------------------------
       CALL CALOWP ( LBOT, WAMNT, RPRES, TEMP, SECANG, WAZOP, WAVGOP,
     $    WAANG, LOPMIN, LOPMAX, LOPUSE, H2OPRD, LOPLOW, DAOP )
C

C      ----------------------------------
C      Calculate the layer transmittances
C      ----------------------------------
C      Calculate TAU for set 1 thru 7
C
       CALL CALT1( INDCHN,  LBOT,  BLMULT,  NCHN1, CLIST1, COEF1,
     $     FIXMUL, CONPRD, FPRED1, WPRED1, OPRED1, TRCPRD,
     $     INDCO2, COFCO2, CO2MLT, INDSO2, COFSO2, SO2MLT,
     $     INDHNO, COFHNO, HNOMLT, INDN2O, COFN2O, N2OMLT,
     $     INDH2O, H2OPRD, COFH2O, LOPMIN, LOPMAX, LOPLOW,
     $     LOPUSE,   WAOP,   DAOP, WAANG,     TAU,   TAUZ)
C
       CALL CALT2( INDCHN, LBOT, BLMULT, NCHN2, CLIST2, COEF2,
     $    FIXMUL, CONPRD, FPRED2, OPRED2, WPRED2, TRCPRD,
     $    INDCO2, COFCO2, CO2MLT, INDSO2, COFSO2, SO2MLT,
     $    INDHNO, COFHNO, HNOMLT, INDN2O, COFN2O, N2OMLT, TAU, TAUZ)
C
       CALL CALT3( INDCHN,   LBOT, BLMULT,  NCHN3, CLIST3,  COEF3,
     $     FIXMUL, CONPRD, FPRED3, MPRED3, WPRED3, TRCPRD,
     $     INDSO2, COFSO2, SO2MLT, INDHNO, COFHNO, HNOMLT,
     $     INDN2O, COFN2O, N2OMLT,
     $     INDH2O, H2OPRD, COFH2O, LOPMIN, LOPMAX, LOPLOW, LOPUSE,
     $       WAOP,   DAOP,  WAANG,    TAU,   TAUZ)
C
       LTAU=.TRUE.
       XZ=1.0
C
       CALL CALT4(  LTAU, INDCHN,   LBOT, BLMULT,  NCHN4, CLIST4,
     $     COEF4, FIXMUL, CONPRD, FPRED4, CPRED4, OPRED4, WPRED4,
     $    TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT,
     $        XZ,    TAU,   TAUZ )
C
       CALL CALT5(  LTAU, INDCHN,   LBOT, BLMULT,  NCHN5, CLIST5,
     $     COEF5, FIXMUL, CONPRD, FPRED5, WPRED5, OPRED5, 
     $    TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT,
     $        XZ,    TAU,   TAUZ )
C
       CALL CALT6(  LTAU, INDCHN,   LBOT, BLMULT,  NCHN6, CLIST6,
     $     COEF6, FIXMUL, CONPRD, FPRED6, WPRED6, OPRED6, TRCPRD,
     $    INDCO2, COFCO2, CO2MLT, INDSO2, COFSO2, SO2MLT,
     $    INDN2O, COFN2O, N2OMLT,     XZ,    TAU,   TAUZ )
C
       CALL CALT7(  LTAU, INDCHN,   LBOT, BLMULT,  NCHN7, CLIST7,
     $     COEF7, FIXMUL, CONPRD, FPRED7, WPRED7, OPRED7,
     $    TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT,
     $        XZ,    TAU,   TAUZ )
C
       IF (DOSUN) THEN
C         ---------------------------------------------
C         Calculate the fast trans predictors *for sun*
C         ---------------------------------------------
C
          CALL SUNPAR ( LBOT,
     $       RTEMP, RWAMNT, ROAMNT, RCAMNT,
     $        TEMP,  WAMNT,  OAMNT,  CAMNT,
     $       RPRES,  SECSUN, CONPRD,
     $       FPRED4, FPRED5, FPRED6, FPRED7,
     $       WPRED4, WPRED5, WPRED6, WPRED7,
     $       OPRED4, OPRED5, OPRED6, OPRED7,
     $       CPRED4, TRCPRD )

C         --------------------------------------------
C         Calculate the layer transmittances *for sun*
C         --------------------------------------------
C
C         Calc fake TAUZSN for sets 1, 2, and 3
          RJUNK1=SUNFDG*SECSUN(LBOT)
          CALL FAKETZ( NFAKE, INDFAK, TAUZ, SECANG(LBOT),
     $       RJUNK1, TAUZSN)
C
C         Calculate TAUZSN for sets 4 thru 7
C
          LTAU=.FALSE.
          XZ=SUNFDG
C
          CALL CALT4( LTAU, INDCHN, LBOT, BLMULT, NCHN4, CLIST4,
     $       COEF4, FIXMUL, CONPRD, FPRED4, CPRED4, OPRED4, WPRED4,
     $       TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT,
     $       XZ, TAU, TAUZSN )
C
          CALL CALT5( LTAU, INDCHN, LBOT, BLMULT, NCHN5, CLIST5,
     $       COEF5, FIXMUL, CONPRD, FPRED5, WPRED5, OPRED5,
     $       TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT,
     $       XZ, TAU, TAUZSN )
C
          CALL CALT6( LTAU, INDCHN, LBOT, BLMULT, NCHN6, CLIST6,
     $       COEF6, FIXMUL, CONPRD, FPRED6, WPRED6, OPRED6,
     $       TRCPRD, INDCO2, COFCO2, CO2MLT, INDSO2, COFSO2, SO2MLT,
     $       INDN2O, COFN2O, N2OMLT, XZ, TAU, TAUZSN )
C
          CALL CALT7( LTAU, INDCHN, LBOT, BLMULT, NCHN7, CLIST7,
     $       COEF7, FIXMUL, CONPRD, FPRED7, WPRED7, OPRED7,
     $       TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT,
     $       XZ, TAU, TAUZSN )
C
       ELSE
C         No sun; set the sun surface-to-space trans to zero
          SUNCOS=0.0
          DO I=1,NCHAN
             TAUZSN(I)=0.0
          ENDDO
       ENDIF
C

C      ---------------------------------------------------
C      Set the emissivity & reflectivity for every channel
C      ---------------------------------------------------
       CALL SETEMS( NCHAN, NEMIS, FREQ, FEMIS, XEMIS,
     $    XRHO, LRHOT, EMIS, RHOSUN, RHOTHR)
C

C      ----------------------
C      Calculate the radiance
C      ----------------------
       CALL CALRAD ( NCHAN, FREQ, TAU, TEMP, TSURF, EMIS, LBOT,
     $    SUNCOS, RHOSUN, DISTES, HSUN, TAUZSN,
     $    SECANG(LBOT), RHOTHR, LABOVE, COEFF, TAUZ, RAD, BT)
C
C      -----------------
C      Calculate non-LTE
C      -----------------
       IF (DOSUN) THEN
          CALL CALNTE ( INDCHN, TEMP, SUNCOS, SCOS1, SECANG(1),
     $       NCHNTE, CLISTN, COEFN, CO2TOP, RAD )
       ENDIF
C
ccc
c      do I=1,NCHAN
c      print *, BT(I), RAD(I)
c      enddo
ccc

C      -------------------
C      Output the radiance
C      -------------------
       CALL WRTRTP(IPROF, IOPCO, NCHAN, RAD, PROF)
C

C      ----------------------
C      End loop over profiles
C      ----------------------
       IPROF=IPROF + 1  ! increment profile counter
       GOTO 10
C

C      -------------------
C      Close the RTP files
C      -------------------
 9999  ISTAT=rtpclose(IOPCI)
       ISTAT=rtpclose(IOPCO)
C
       STOP
       END
