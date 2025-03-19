program main

! SARTA main program for regression based fast coefficients
!       
! May 2024. CLH: First source code conversion to Fortran 90 from 77
!
! Jul 2024 CLH: includes option for basic (0-90.deg) nonLTE and extended
!               (0-120.deg) nonLTE using parameters in the include file.
!               and dependencies in. rdcoef_reg, calxnte_reg

use incFTC
!use cbplev

!
implicit none 
!
include 'rtpdefs.f90'

!---------------------------------------------------------------
!      EXTERNAL FUNCTIONS
!---------------------------------------------------------------
real(4) :: VACONV
real(4) :: SACONV
integer :: rtpclose
integer :: rtpopen

!---------------------
!  used locally only
! --------------------
integer :: I, L             ! loop counters
real(4) :: EVA                     ! (Earth) local viewing angle
real(4) :: ANGMAX                  ! maximum allowed viewing angle
real(4) :: RJUNK1, RJUNK2          ! junk/work
REAL(4) :: CO2PPM                  ! Profile mean dry air CO2 mixing ratio
REAL(4), dimension(MAXLAY) :: PLAY   ! layer mean pressure
! ------------------
! For rdinfo
! ------------------
character(len=120) :: FIN, FOUT
integer :: NWANTP                      ! no. wanted profiles
integer, dimension(MAXPRO) :: LISTP    ! profile list numbers
logical :: LRHOT                       ! force RHO for refl thermal?
!LISTP = [1,2]

!     Structures (see "rtpdefs.f")
integer :: ISTAT
RECORD /RTPPROF/ PROF                      ! profile data
RECORD /RTPHEAD/ HEAD                      ! header data
RECORD /RTPATTR/ HATT(MAXNATTR)            ! header attributes
RECORD /RTPATTR/ PATT(MAXNATTR)            ! profile attributes
!  Boundary pressure levels
COMMON /COMLEV/ PLEV
REAL(4), dimension(MAXLAY+1) :: PLEV

! -------------------------------
!  for FAKETZ
! -------------------------------
integer :: NFAKE                      ! # of channels to "fake"
integer, dimension(MXCHAN) :: INDFAK  ! indices of channels to fake

! ---------------------------------------
!      for RDPREF; reference profile
! ---------------------------------------
!  RPNAM ! ref prof name/ID
!   RALT ! ref prof layer altitude
!    RDZ ! ref prof layer thickness
!  RPRES ! ref prof layer average pressure
!  RTEMP ! ref prof layer average temperature
! RFAMNT ! ref prof layer "fixed" (CO2) amount
! RWAMNT ! ref prof layer water (H2O) amount
! ROAMNT ! ref prof layer ozone (O3) amount
! RCAMNT ! ref prof layer carbon monoxide (CO) amount
! RMAMNT ! ref prof layer methane (CH4) amount
! RSAMNT ! ref prof layer sulfer dioxide (SO2) amount
! RHAMNT ! ref prof layer nitric acid (HNO3) amount
! RNAMNT ! ref prof layer nitrous oxide (N2O) amount
! RAAMNT ! ref prof layer ammonia (NH3) amount
! RDAMNT ! ref prof layer HDO (HDO) amount (uses water)
real(4),dimension(MAXLAY) :: RALT, RDZ, RPRES, RTEMP, RFAMNT, RWAMNT, ROAMNT
real(4),dimension(MAXLAY) :: RCAMNT, RMAMNT, RSAMNT, RHAMNT, RNAMNT, RAAMNT
CHARACTER(len=40) :: RPNAM             ! ref prof name/ID

! --------------------------------------------------------
!    for OPNRTP
! -------------------------------------------------------
! PTYPE                       ! profile type
! NCHAN                       ! # of selected channels
! FCHAN                       ! chan center frequency
! LSTCHN                      ! list of selected channels
! INDCHN                      ! array indices for all channels
! IH2O,IO3,ICO,ICH4,ICO2,ISO2,IHNO3,IN2O, INH3  ! index of XXX gas in gamnt
! IOPCI, IOPCO                ! input, output RTP unit
! LCO2PM                      ! CO2 profile in ppmv?
integer :: PTYPE, NCHAN, IH2O, IO3, ICO, ICH4, ICO2, ISO2, IHNO3, IN2O
integer :: INH3, IHDO, IOPCI, IOPCO
real(4), dimension(MXCHAN) :: FCHAN
integer, dimension(MXCHAN) :: LSTCHN, INDCHN
logical :: LCO2PM
! -------------------------------------------
!      for RDCOEF
! -------------------------------------------
integer :: NCHN1, NCHN2, NCHN3, NCHN4, NCHN5, NCHN6, NCHN7
integer :: NCHNTE
integer, dimension(MXCHAN) :: LABOVE             ! chan downwel thermal layer above
integer, dimension(MXCHAN) :: SETCHN             ! set # for each channel
integer, dimension(MXCHN1) :: CLIST1
integer, dimension(MXCHN2) :: CLIST2
integer, dimension(MXCHN3) :: CLIST3
integer, dimension(MXCHN4) :: CLIST4
integer, dimension(MXCHN5) :: CLIST5
integer, dimension(MXCHN6) :: CLIST6
integer, dimension(MXCHN7) :: CLIST7
integer, dimension(MXCNTE) :: CLISTN
integer, dimension(MXCHAN) :: INDCO2
integer, dimension(MXCHAN) :: INDSO2
integer, dimension(MXCHAN) :: INDHNO
integer, dimension(MXCHAN) :: INDN2O
integer, dimension(MXCHAN) :: INDNH3
integer, dimension(MXCHAN) :: INDHDO
integer, dimension(MXCHAN) :: INDH2O
real(4), dimension(N1COEF, MAXLAY, MXCHN1) :: COEF1
real(4), dimension(N2COEF, MAXLAY, MXCHN2) :: COEF2
real(4), dimension(N3COEF, MAXLAY, MXCHN3) :: COEF3
real(4), dimension(N4COEF, MAXLAY, MXCHN4) :: COEF4
real(4), dimension(N5COEF, MAXLAY, MXCHN5) :: COEF5
real(4), dimension(N6COEF, MAXLAY, MXCHN6) :: COEF6
real(4), dimension(N7COEF, MAXLAY, MXCHN6) :: COEF7
real(4), dimension(NCO2, MAXLAY, MXCHNC) :: COFCO2
real(4), dimension(NSO2, MAXLAY, MXCHNS) :: COFSO2
real(4), dimension(NHNO3, MAXLAY, MXCHNH) :: COFHNO
real(4), dimension(NNH3, MAXLAY, MXCHNA) :: COFNH3
real(4), dimension(NN2O, MAXLAY, MXCHNN) :: COFN2O
real(4), dimension(NHDO, MAXLAY, MXCHND) :: COFHDO
real(4), dimension(NH2O, MXOWLY, MXCHNW) :: COFH2O
real(4), dimension(NFCOEF, MXCHAN) :: COEFF         ! channel "f" factor
real(4), dimension(XNCOEF,MXCNTE) :: COEFN          ! non-LTE coefficients
real(4), dimension(MXOWLY) :: WAZOP                 ! optran l-to-s amounts
real(4), dimension(NOWAVG, MXOWLY) :: WAVGOP        ! optran raw predictor averages
real(4), dimension(MAXLAY) :: FX                    ! fixed gas adjustment
real(4), dimension(MXCHAN) :: FREQ          

! --------------------------------
! For rd_xnte_ann
! -------------------------------
!!real(4), dimension(NCHNTE) :: IP_YMAX, IP_YMIN, OP_YMAX, OP_YMIN, OP_XMAX, OP_XMIN
!!real(4), dimension(NCHNTE) :: FCHANN
!!real(4), dimension(NCHNTE,4) :: B2, IP_XMAX, IP_XMIN
!!real(4), dimension(NCHNTE,NNNNTE) :: B1, LW
!!real(4), dimension(NCHNTE,NNNNTE,4):: IW
!!integer, dimension(NCHNTE) :: INDNTE, CLISTN ! was ICHANN (CLISTN)
!
integer :: IPROF,IERR,ich                     ! Atmospheric Profile counter
! --------------------------------------------------
!  For tunmlt
! --------------------------------------------------

! ----------------------------
! for rdsun
! ---------------------------

!  Other variables for the sun
real(4) :: SUNANG                        ! solar zenith angle (at 0 altitude)
real(4) :: SZALAY                        ! solar zenith angle in some layer
real(4) :: SUNCOS                        ! cosine of sun zenith angle
real(4) :: SCOS1                         ! cosine of sun zenith angle at layer1
real(4) :: SUNFDG                        ! fudge factor for large solar angles
real(4), dimension(MAXLAY) :: SECSUN     ! secant of effective sun local path angle
!       REAL DISTES         ! distance of Earth from the sun
real(4), dimension(MXCHAN) :: TAUZSN     ! chan eff sun angle surface-to-space trans
logical :: DOSUN                         ! do sun calc?
!       REAL XUNANG         ! modified solar zenith for extended nonLTE.
!       REAL XCOS1          ! cosine of mod. solzen passed to CALNTE
!       REAL XZALAY         ! modified solar zenith in other layer
real(4), dimension(MXCHAN) :: HSUN        ! sun radiance (direct from sun)

! -------------------------------------------
! for RDRTP; profile to calculate
! -------------------------------------------
!  NLAY     number of layers in profile
!  LON,LON  prof longitude, latitude
!  ALT      prof layer altitudes
!  TEMP     prof layer average temperature
!  WAMNT    prof layer water (H2O) amount
!  OAMNT    prof layer ozone (O3) amount
!  CAMNT    prof layer carbon monoxide (CO) amount
!  MAMNT    prof layer methane (CH4) amount
!  FAMNT    prof layer CO2 amount
!  SAMNT    prof layer SO2 amount
!  HAMNT    prof layer HNO3 amount
!  NAMNT    prof layer N2O amount
!  AAMNT    prof layer ammonia (NH3) amount
integer :: NLAY
real(4) :: LAT, LON
real(4),dimension(MAXLAY) :: ALT, TEMP, WAMNT, OAMNT, CAMNT, MAMNT
real(4),dimension(MAXLAY) :: FAMNT, SAMNT, HAMNT, NAMNT, AAMNT
real(4) :: HDODPL         ! HDO depletion from prof.udef(20,:)
logical :: LWANT          ! do we want this profile?

! -------------------------------
!      for MEAN_T
! -------------------------------
real(4), dimension(MAXLAY) :: TPSEUD

! -----------------------------------
!   for CALPAR
! -----------------------------------
logical :: LCO2, LN2O, LSO2, LNH3, LHDO, LHNO3 ! trace gas profile switch
real(4), dimension(MAXLAY) :: SECANG           ! local path angle secant
real(4), dimension(MAXLAY) :: FIXMUL           ! "fixed" amount multiplier (~1)
real(4), dimension(N1CON,MAXLAY) :: CONPRD     ! water continuum predictors
real(4), dimension(N1FIX,MAXLAY) :: FPRED1     ! setN 'fixed' predictors
real(4), dimension(N2FIX,MAXLAY) :: FPRED2
real(4), dimension(N3FIX,MAXLAY) :: FPRED3
real(4), dimension(N4FIX,MAXLAY) :: FPRED4
real(4), dimension(N5FIX,MAXLAY) :: FPRED5
real(4), dimension(N6FIX,MAXLAY) :: FPRED6
real(4), dimension(N7FIX,MAXLAY) :: FPRED7
real(4), dimension(N1H2O,MAXLAY) :: WPRED1
real(4), dimension(N2H2O,MAXLAY) :: WPRED2
real(4), dimension(N3H2O,MAXLAY) :: WPRED3
real(4), dimension(N4H2O,MAXLAY) :: WPRED4
real(4), dimension(N5H2O,MAXLAY) :: WPRED5
real(4), dimension(N6H2O,MAXLAY) :: WPRED6
real(4), dimension(N7H2O,MAXLAY) :: WPRED7
real(4), dimension(N1O3,MAXLAY) :: OPRED1
real(4), dimension(N2O3,MAXLAY) :: OPRED2
real(4), dimension(N4O3,MAXLAY) :: OPRED4
real(4), dimension(N5O3,MAXLAY) :: OPRED5
real(4), dimension(N6O3,MAXLAY) :: OPRED6
real(4), dimension(N7O3,MAXLAY) :: OPRED7
real(4), dimension(N3CH4,MAXLAY) :: MPRED3
real(4), dimension(N4CO,MAXLAY) :: CPRED4
real(4), dimension(NTRACE,MAXLAY) :: TRCPRD
real(4), dimension(NHDO,MAXLAY) :: DPRED
!       REAL CONPRD( N1CON,MAXLAY) ! water continuum predictors
!       REAL FPRED1( N1FIX,MAXLAY) ! set1 "fixed" predictors
!       REAL FPRED2( N2FIX,MAXLAY) ! set2 "fixed" predictors
!       REAL FPRED3( N3FIX,MAXLAY) ! set3 "fixed" predictors
!       REAL FPRED4( N4FIX,MAXLAY) ! set4 "fixed" predictors
!       REAL FPRED5( N5FIX,MAXLAY) ! set5 "fixed" predictors
!       REAL FPRED6( N6FIX,MAXLAY) ! set6 "fixed" predictors
!       REAL FPRED7( N7FIX,MAXLAY) ! set7 "fixed" predictors
!       REAL WPRED1( N1H2O,MAXLAY) ! set1 water predictors
!       REAL WPRED2( N2H2O,MAXLAY) ! set2 water predictors
!       REAL WPRED3( N3H2O,MAXLAY) ! set3 water predictors
!       REAL WPRED4( N4H2O,MAXLAY) ! set4 water predictors
!       REAL WPRED5( N5H2O,MAXLAY) ! set5 water predictors
!       REAL WPRED6( N6H2O,MAXLAY) ! set6 water predictors
!       REAL WPRED7( N7H2O,MAXLAY) ! set7 water predictors
!       REAL  DPRED(  NHDO,MAXLAY) ! HDO perturbation predictors
!       REAL OPRED1(  N1O3,MAXLAY) ! set1 ozone predictors
!       REAL OPRED2(  N2O3,MAXLAY) ! set2 ozone predictors
!       REAL OPRED4(  N4O3,MAXLAY) ! set4 ozone predictors
!       REAL OPRED5(  N5O3,MAXLAY) ! set5 ozone predictors
!       REAL OPRED6(  N6O3,MAXLAY) ! set6 ozone predictors
!       REAL OPRED7(  N7O3,MAXLAY) ! set7 ozone predictors
!       REAL MPRED3( N3CH4,MAXLAY) ! set3 methane predictors
!       REAL CPRED4(  N4CO,MAXLAY) ! set4 carbon monoxide predictors
!       REAL TRCPRD(NTRACE,MAXLAY) ! trace gas pert perdictors
!  trace gas perturbation multipliers
real(4), dimension(MAXLAY) :: CO2MLT, SO2MLT, HNOMLT, N2OMLT, NH3MLT
real(4), dimension(MAXLAY) :: HDOMLT
real(4) :: CO2TOP                   ! top layers CO2 mixing ratio 

! --------------------------------
!  for surface
! --------------------------------
!   LBOT             ! bottom layer index number
!  NEMIS             ! # of emis pts
!  PSURF             ! surface pressure
! BLMULT             ! bottom layer fractional multiplier
integer :: NEMIS, LBOT
real(4) :: PSURF, BLMULT
REAL(4),dimension(MXEMIS) :: FEMIS,XEMIS,XRHO  ! emis & reflec freq pts

! -----------------------------------------------
!      for CALOWP
! -----------------------------------------------
integer :: LOPMIN, LOPMAX
integer, dimension(MAXLAY) :: LOPLOW
real(4), dimension(MAXLAY) :: WAANG, DAOP
real(4), dimension(  NH2O,MXOWLY) :: H2OPRD
logical, dimension(MXOWLY) :: LOPUSE

! ------------------------------------------------
!    for CALT
! ------------------------------------------------
real(4), dimension(MAXLAY,MXCHAN) :: TAU ! chan layer effective trans
real(4), dimension(MXCHAN) :: TAUZ       ! chan surface-to-space trans
real(4), dimension(MXOWLY) :: WAOP       ! OPTRAN abs coef scaling factor
real(4) :: XZ                            ! optical depth multiplier for TAUZ
logical :: LTAU                          ! Calc all layer transmittances?

! ---------------------------
!      for CALRAD
! ---------------------------
!       REAL  TSURF         ! surface temperature
real(4), dimension(MXCHAN) :: EMIS, RHOSUN, RHOTHR, RAD, BT
! EMIS      ! chan surface emissivity
! RHOSUN    ! chan reflectivity for sun
! RHOTHR    ! chan reflectivity for downwelling thermal
! RAD       ! chan radiance
! BT        ! chan brightness temperature
real(4) :: TSURF

! ----------------------------
! for satellite viewing angle
! ---------------------------
! SATANG      ! input satellite scan angle (degrees)
! SATZEN      ! input satellite zenith angle (degrees)
! SALT        ! input satellite altitude (kilometers)
! SVA         ! satellite viewing angle (degrees)
real(4) :: SATANG, SATZEN, SALT, SVA
! --------------------------
! for read_r49_regdata
! --------------------------
!real(4),dimension(NCHNTE,5358) :: DELTAR
!real(4),dimension(4,5358) :: PRDNTE

! ----------------------------------------
! for calxnte_nn
! ----------------------------------------
real(4) :: XALT, VSEC1
real(4), dimension(MXCHAN) :: DRAD

  if(DEBUG) print*,'main: completed initialization'

! *****************************************************************************
! *****************************************************************************

! =============================================================
! start main pipeline
! =============================================================

!  ---------
!   Calc PLAY
!  ---------
!  Mean layer pressure (KLAYERS definition)
   DO L=1,MAXLAY
      PLAY(L) = ( PLEV(L+1) - PLEV(L) )/LOG( PLEV(L+1)/PLEV(L) )
   ENDDO
!!write(*,*) 'Enter to continue'
!!read(*,*)
!  -----------------------------
!  Read in the reference profile
!  -----------------------------
CALL rdpref(IOUN, FNPREF, RPNAM, RALT, RDZ, RPRES, RTEMP, &
         RFAMNT, RWAMNT, ROAMNT, RCAMNT, RMAMNT, RSAMNT, &
         RHAMNT, RNAMNT, RAAMNT)

if (DEBUG) print*, 'sarta: completed call rdprof'

! ------------------------------------------
!  Read command line inputs
! ------------------------------------------
call rdinfo(FIN, FOUT, LRHOT, NWANTP, LISTP)

  if(DEBUG) write(6,*) 'main: NWANTP ', NWANTP
!
call opnrtp(FIN, LRHOT, PTYPE, NCHAN, FCHAN, LSTCHN, INDCHN, &
            IH2O, IO3, ICO, ICH4, ICO2, ISO2, IHNO3, IN2O, INH3, &
            IOPCI, HEAD, HATT, PATT, LCO2PM)
!
! write(*,*) 'Main: Enter to continue'
! read(*,*)
! call rdcoef()
   CALL RDCOEF( IOUN, NCHAN, INDCHN, SETCHN, &
       NCHN1,  NCHN2,  NCHN3,  NCHN4,  NCHN5,  NCHN6,  NCHN7, &
      CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7, &
       COEF1,  COEF2,  COEF3,  COEF4,  COEF5,  COEF6,  COEF7, &
        FREQ, LABOVE,  COEFF, INDCO2, COFCO2, INDSO2, COFSO2, &
      INDHNO, COFHNO, INDN2O, COFN2O, INDNH3, COFNH3, INDHDO, &
      COFHDO, INDH2O,  WAZOP, WAVGOP, COFH2O, FX, NCHNTE, CLISTN, COEFN )

! call read_nlte_ann
!   if (COFNTE) then
!      call read_nlte_ann(IP_YMAX, IP_YMIN, IP_XMAX, IP_XMIN,&
!          B1, B2, IW, LW, OP_YMAX, OP_YMIN, OP_XMAX, &
!          OP_XMIN,CLISTN,FCHANN )        ! ICHANNN -> INDNTE = CLISTN
!   endif

! call tunmlt()
   call tunmlt(IOUN, NCHAN, INDCHN, SETCHN, &
          NCHN1,  NCHN2,  NCHN3,  NCHN4,  NCHN5,  NCHN6,  NCHN7, &
         CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7, &
          COEF1,  COEF2,  COEF3,  COEF4,  COEF5,  COEF6,  COEF7, &
           FREQ, LABOVE, COEFF,  INDCO2, COFCO2, INDSO2, COFSO2, &
         INDHNO, COFHNO, INDN2O, COFN2O, &
         INDH2O,  WAZOP, WAVGOP, COFH2O, FX)  ! , NCHNTE, CLISTN, COEFN )
!
!  Calc OPTRAN absorption coefficient scaling factor WAOP
   WAOP(1)=WAZOP(1)
   DO L=2,MXOWLY
       WAOP(L)=1.00*(WAZOP(L) - WAZOP(L-1))
!        WAOP(L)=1.014*(WAZOP(L) - WAZOP(L-1))
   ENDDO
!
! call rdsun()
    call rdsun(IOUN, INDCHN, HSUN)
!    
! ------------------------------------------------
!    open the output RTP file ready for wrtrtp()
! ------------------------------------------------
  ISTAT = rtpopen(FOUT,'c',HEAD,HATT,PATT,IOPCO)
!
! NFAKE
!  -----------------------------------------------
!  All channels from sets 1, 2, and 3 are to use a
!  fake effective sun angle layer-to-space trans
!  -----------------------------------------------
       NFAKE=0
! 
       DO I=1,NCHN1
          NFAKE=NFAKE + 1
          INDFAK(NFAKE)=INDCHN( CLIST1(I) )
       ENDDO
!
       DO I=1,NCHN2
          NFAKE=NFAKE + 1
          INDFAK(NFAKE)=INDCHN( CLIST2(I) )
       ENDDO
!
       DO I=1,NCHN3
          NFAKE=NFAKE + 1
          INDFAK(NFAKE)=INDCHN( CLIST3(I) )
       ENDDO
!
! Check FREQ vs FCHAN
! --------------------------
!    Check FREQ and FCHAN
!  -------------------------
!   Note: FREQ comes the coef data, while FCHAN comes
!    from the input RTP file read by OPNRTP.  It is possible
!    that FCHAN is "nodata", so we check the first element.
   IF (FCHAN(1) .GT. 640 .AND. FCHAN(1) .LT. 2670) THEN
      DO I=1,NCHAN
         RJUNK1=ABS(FREQ(I) - FCHAN(I))
         RJUNK2=0.01*FREQ(I)/1200.0   ! ~1% of a channel fullwidth
         IF (RJUNK1 .GT. RJUNK2) THEN
            WRITE(IOINFO,1010) I, LSTCHN(I), FREQ(I), FCHAN(I)
 1010       FORMAT('Warning! index=',I4,', chan ID=',I4, &
            ', fastmodel freq=',F8.3,', RTP freq=',F8.3)
         ENDIF
         HEAD%vchan(I)=FREQ(I)
      ENDDO
   ELSE
      DO I=1,NCHAN
         HEAD%vchan(I)=FREQ(I)
      ENDDO
   ENDIF
!
! ***************************************************************************
!   ---------------------------------
!      Start of loop over profiles
!   ---------------------------------
! ***************************************************************************
     IPROF=1  ! initialize profile counter
!    Do you want this profile?
 10  LWANT=.TRUE.
     IF (NWANTP .GT. 1) THEN
!    Look for current profile on list of wanted profiles
        LWANT=.FALSE.
        DO I=1,NWANTP
           IF (IPROF .EQ. LISTP(I)) LWANT=.TRUE.
        ENDDO
     ENDIF

! --------------
! Read input RTP
! --------------
     CALL RDRTP( LWANT, IPROF, IOPCI, &
         IH2O, IO3, ICO, ICH4, ICO2, ISO2, IHNO3, IN2O, INH3, &
         PTYPE, RALT, LCO2PM, &
         NLAY, NEMIS, LAT, LON, SATANG, SATZEN, SALT, SUNANG, &
         PSURF, TSURF, CO2PPM, HDODPL, FEMIS, XEMIS, XRHO, &
         TEMP, WAMNT, OAMNT, CAMNT, MAMNT, FAMNT, SAMNT, HAMNT, NAMNT, &
         AAMNT, ALT, PROF, ISTAT )

     IF (ISTAT .EQ. -1)  GOTO 9999  ! reached End Of File

     IF (.NOT. LWANT) THEN
!       Skip this profile
        IPROF=IPROF+ 1
        GOTO 10
     ENDIF

      if (DEBUG) write(6,2010) IPROF
 2010 FORMAT('sarta RDRTP IPROF: ', I5)

! ------------------------------------------------
! check pressure levels from rtp and refprof match
! ------------------------------------------------
!   CALL CHECK_PLEVS( PROF, NLAY )
!
! ----------------------------------------
!   Determine bottom layer, CO2, & angles
!  ---------------------------------------
     call GETBOT(NLAY, PLEV, PSURF, LBOT, BLMULT)
!
  if(DEBUG) write(6,*) 'main: completed getbot'

! ------------------------------------------------
! If minor gas present turn switch on for compute.
! ------------------------------------------------
!    CO2 profile switch
     IF (ICO2 .LT. 1) THEN
        LCO2=.FALSE.
     ELSE
        LCO2=.TRUE.
     ENDIF
!   N2O profile switch
     IF (IN2O .LT. 1) THEN
        LN2O=.FALSE.
     ELSE
        LN2O=.TRUE.
     ENDIF
!   SO2 profile switch
     IF (ISO2 .LT. 1) THEN
        LSO2=.FALSE.
     ELSE
        LSO2=.TRUE.
     ENDIF
!   NH3 profile switch
     IF (INH3 .LT. 1) THEN
        LNH3=.FALSE.
     ELSE
        LNH3=.TRUE.
     ENDIF
!   HNO3 profile switch
     IF (IHNO3 .LT. 1) THEN
        LHNO3=.FALSE.
     ELSE
        LHNO3=.TRUE.
     ENDIF
!   HDO switch (default .TRUE. from water)
     LHDO=.TRUE.
!
! Check PTYPE and calc partial bottom layer
     IF (PTYPE .EQ. AIRSLAY) THEN
!     Copy pseudo level temperatures to another array
        DO I=1,LBOT
           TPSEUD(I)=TEMP(I)
        ENDDO
! ------------------------------------
!     Convert temperatures
! ------------------------------------
        CALL MEAN_T(LBOT, PLEV, PSURF, TPSEUD, TEMP)
     ELSE
!    Calc mean pressure for bottom fractional layer
        RJUNK1 = ( PSURF - PLEV(LBOT) )/LOG( PSURF/PLEV(LBOT) )
!    Do interpolation for fractional bottom layer mean temperature
!    assuming T is in linear in log(P)
        RJUNK2=( TEMP(LBOT) - TEMP(LBOT-1) )/ &
            LOG( PLAY(LBOT)/PLAY(LBOT-1) )             ! slope
        TEMP(LBOT)=RJUNK2*LOG( RJUNK1/PLAY(LBOT-1) ) + TEMP(LBOT - 1)
     ENDIF

! ----------------------------------
!   Check satellite elevation
! ----------------------------------
     IF (SALT .GT. 0) THEN
!     Warn and use default if invalid
        IF (SALT .LT. XSALT-150 .OR. SALT .GT. XSALT+150) THEN
!       IF (SALT .LT. XSALT-50 .OR. SALT .GT. XSALT+50) THEN
           WRITE(IOINFO,1020) IPROF, SALT, XSALT
 1020      FORMAT('Warning! Profile',I5, &
             ': replacing invalid input satellite altitude ', &
             1PE11.4,' with default ',1PE11.4,' km')
           SALT=XSALT
        ENDIF
     ELSE
        SALT=XSALT
     ENDIF
! ----------------------------------------------
!      Convert SATZEN or SATANG to viewing angle
! -----------------------------------------------
     IF (SATZEN .GE. 0 .AND. SATZEN .LT. 63) THEN
!    Convert zenith angle (deg) at surface to view angle at satellite
        SVA=SACONV( SATZEN, SALT*1000 )/DEG2RAD
     ELSE
!    Check if scan angle is valid
       IF (SATANG .GT. -49.6 .AND. SATANG .LT. 49.6) THEN
!      View angle should be within a few degrees of scan angle
          SVA=ABS( SATANG )
       ELSE
          WRITE(IOERR,1030) IPROF, SATZEN, SATANG
 1030     FORMAT('Error! Profile',I5, &
               ': invalid angles for SATZEN ',1PE11.4, &
               ' and SATANG ',E11.4) 
          STOP
       ENDIF
     ENDIF
 if(DEBUG) write(6,*) 'main: completed satzen satang to view ang'
! --------------------------
! truncate view angle
! --------------------------
     ANGMAX=53  ! max satellite view angle (49.5 scan + 3.5 spacecraft)
     IF (SVA .GT. ANGMAX) THEN
!    Truncate angle if too big
       WRITE(IOINFO,1040) IPROF, SVA
 1040  FORMAT('Warning! Profile',I5,': truncating view angle ', &
            1PE11.4,' to 53 degrees')
       SVA=ANGMAX
     ENDIF
! --------------------------------------------------------------
!   Convert from satellite to earth viewing angle (in radians)
! --------------------------------------------------------------
     DO L=1,LBOT
        EVA=VACONV(SVA, SALT, ALT(L))
        SECANG(L)=1.0E+0/COS(EVA)       ! EVA units: radians
!!       for testing
!!       SECANG(L)=SVA
     ENDDO
! --------------------------------
!      Calc total sun angle secant
! --------------------------------
     DOSUN=.FALSE.
!!    XUNANG = SUNANG*90/120
!!    IF (XUNANG .GE. 0.0 .AND. XUNANG .LT. 89.9) DOSUN=.TRUE.
     IF (SUNANG .GE. 0.0 .AND. SUNANG .LT. 89.9) DOSUN=.TRUE.
     IF (DOSUN) THEN
       SUNCOS=COS(DEG2RAD*SUNANG)
!!       XUNCOS=COS(CONV*XUNANG)
       SZALAY=SACONV(SUNANG,ALT(1))
!!       XZALAY=SACONV(XUNANG,ALT(1))
       SCOS1=COS(SZALAY)
!!       XCOS1=COS(XZALAY)
       RJUNK2=SECANG(LBOT) + 1.0/SUNCOS ! Total secant
!
!      Calc non-unity fudge factor if total secant > 9
       IF (RJUNK2 .GT. 9.0) THEN
!      fudge factor = true_total_secant/calc_total_secant
          SUNFDG=RJUNK2/9.0
!         truncated solar angle (deg) to use to calc SECSUN
          RJUNK1=ACOS( 1.0/(9.0 - SECANG(LBOT)) )/DEG2RAD
       ELSE
          SUNFDG=1.0
          RJUNK1=SUNANG
       ENDIF
!
       DO L=1,LBOT
          SZALAY=SACONV(RJUNK1,ALT(L))
          SECSUN(L)=SECANG(L) + 1.0E+0/COS(SZALAY)
       ENDDO

     ENDIF

  if(DEBUG) write(6,*) 'main: completed view and sun angle calcs'
!
!  -----------------------------------
!  Calculate the fast trans predictors
!  -----------------------------------
     CALL CALPAR (LBOT, &
         RTEMP,RFAMNT, RWAMNT,ROAMNT,RCAMNT,RMAMNT,RSAMNT,RHAMNT,RNAMNT, &
        RAAMNT,  TEMP,  FAMNT, WAMNT, OAMNT, CAMNT, MAMNT, SAMNT, HAMNT, &
         NAMNT, AAMNT,  RPRES,SECANG,HDODPL,   LAT,    FX,   RDZ, &
        LCO2, LN2O, LSO2,LNH3,LHDO, LHNO3,LCO2PM, CO2PPM,CO2TOP, &
        FIXMUL,CONPRD,DPRED, &
        FPRED1,FPRED2,FPRED3,FPRED4,FPRED5,FPRED6,FPRED7, &
        WPRED1,WPRED2,WPRED3,WPRED4,WPRED5,WPRED6,WPRED7, &
        OPRED1,OPRED2,       OPRED4,OPRED5,OPRED6,OPRED7, &
        MPRED3,CPRED4,TRCPRD,CO2MLT,SO2MLT,HNOMLT,N2OMLT,NH3MLT,HDOMLT)
!
!   write(6,*) 'main: completed calpar'
!   -------------------------------------
!    Calculate the OPTRAN H2O predictors
!  -- -----------------------------------
!    call CALOWP()
     IF (CFOPTR) THEN
       CALL CALOWP ( LBOT, WAMNT, RPRES, TEMP, SECANG, WAZOP, WAVGOP, &
           WAANG, LOPMIN, LOPMAX, LOPUSE, H2OPRD, LOPLOW, DAOP )
     ENDIF
!
! call CALT{1..7}()
!  ----------------------------------
!   Calculate the layer transmittances
!  ----------------------------------
!
     CALL CALT1( INDCHN,  NLAY,  BLMULT,  NCHN1, CLIST1, COEF1, &
          FIXMUL, CONPRD, FPRED1, WPRED1, DPRED,  OPRED1, TRCPRD, &
          INDCO2, COFCO2, CO2MLT, INDSO2, COFSO2, SO2MLT, &
          INDHNO, COFHNO, HNOMLT, INDN2O, COFN2O, N2OMLT, &
          INDNH3, COFNH3, NH3MLT, INDHDO, COFHDO, HDOMLT, &
          INDH2O, H2OPRD, COFH2O, LOPMIN, LOPMAX, LOPLOW, &
          LOPUSE,   WAOP,   DAOP, WAANG,     TAU,   TAUZ)
     if(DEBUG) write(6,'(A)') 'sarta: completed CALT1'
!
     CALL CALT2( INDCHN, NLAY, BLMULT, NCHN2, CLIST2, COEF2, &
         FIXMUL, CONPRD, FPRED2, OPRED2, WPRED2, DPRED, TRCPRD, &
         INDCO2, COFCO2, CO2MLT, INDSO2, COFSO2, SO2MLT, &
         INDHNO, COFHNO, HNOMLT, INDN2O, COFN2O, N2OMLT,  &
         INDNH3, COFNH3, NH3MLT, INDHDO, COFHDO, HDOMLT,TAU, TAUZ)
     if(DEBUG)  print*, 'sarta: completed CALT2'
!
     CALL CALT3( INDCHN,   NLAY, BLMULT,  NCHN3, CLIST3,  COEF3, &
          FIXMUL, CONPRD, FPRED3, MPRED3, WPRED3, DPRED,  TRCPRD, &
          INDSO2, COFSO2, SO2MLT, INDHNO, COFHNO, HNOMLT, &
          INDN2O, COFN2O, N2OMLT, INDNH3, COFNH3, NH3MLT, &
          INDHDO, COFHDO, HDOMLT, INDH2O, H2OPRD, COFH2O, &
          LOPMIN, LOPMAX, LOPLOW, LOPUSE, &
          WAOP,   DAOP,   WAANG,  TAU,    TAUZ)
     if(DEBUG) write(6,'(A)') 'sarta: completed CALT3'
!
     LTAU=.TRUE.
     XZ=1.0
!
     CALL CALT4(  LTAU, INDCHN,   LBOT, BLMULT,  NCHN4, CLIST4, &
          COEF4, FIXMUL, CONPRD, FPRED4, CPRED4, OPRED4, WPRED4, &
         TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT, &
             XZ,    TAU,   TAUZ )
     if(DEBUG) write(6,'(A)') 'sarta: completed CALT4'
!
     CALL CALT5(  LTAU, INDCHN,   LBOT, BLMULT,  NCHN5, CLIST5, &
          COEF5, FIXMUL, CONPRD, FPRED5, WPRED5, OPRED5, &
         TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT, &
             XZ,    TAU,   TAUZ )
     if(DEBUG) write(6,'(A)') 'sarta: completed CALT5'
!
     CALL CALT6(  LTAU, INDCHN,   LBOT, BLMULT,  NCHN6, CLIST6, &
        COEF6, FIXMUL, CONPRD, FPRED6, WPRED6, OPRED6, DPRED, TRCPRD, &
       INDCO2, COFCO2, CO2MLT, INDSO2, COFSO2, SO2MLT, &
       INDN2O, COFN2O, N2OMLT, INDHDO, COFHDO, HDOMLT, &
       XZ,     TAU,    TAUZ )
     if(DEBUG) write(6,'(A)') 'sarta: completed CALT6'
!
     CALL CALT7(  LTAU, INDCHN,   LBOT, BLMULT,  NCHN7, CLIST7, &
        COEF7, FIXMUL, CONPRD, FPRED7, WPRED7, OPRED7, DPRED, &
        TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT, &
        INDHDO, COFHDO, HDOMLT, XZ,    TAU,   TAUZ )
!
!  write(6,*) 'main: completed calt1-7'
! call SUNPAR()
     IF (DOSUN) THEN
!   ---------------------------------------------
!    Calculate the fast trans predictors *for sun*
!   ---------------------------------------------
         CALL SUNPAR ( LBOT, &
            RTEMP, RWAMNT, ROAMNT, RCAMNT, &
             TEMP,  WAMNT,  OAMNT,  CAMNT, &
            RPRES,  SECSUN, CONPRD, &
            FPRED4, FPRED5, FPRED6, FPRED7, &
            WPRED4, WPRED5, WPRED6, WPRED7, &
            OPRED4, OPRED5, OPRED6, OPRED7, &
            CPRED4, TRCPRD )
!         if (DEBUG)  print*, 'sarta: completed SUNPAR'
!   --------------------------------------------
!   Calculate the layer transmittances *for sun*
!   --------------------------------------------
!
!  Calc fake TAUZSN for sets 1, 2, and 3
          RJUNK1=SUNFDG*SECSUN(LBOT)
          CALL FAKETZ( NFAKE, INDFAK, TAUZ, SECANG(LBOT), &
            RJUNK1, TAUZSN)
!
!     Calculate TAUZSN for sets 4 thru 7
!
          LTAU=.FALSE.
          XZ=SUNFDG
!
          CALL CALT4( LTAU, INDCHN, LBOT, BLMULT, NCHN4, CLIST4, &
            COEF4, FIXMUL, CONPRD, FPRED4, CPRED4, OPRED4, WPRED4, &
            TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT, &
            XZ, TAU, TAUZSN )
!
          CALL CALT5( LTAU, INDCHN, LBOT, BLMULT, NCHN5, CLIST5, &
            COEF5, FIXMUL, CONPRD, FPRED5, WPRED5, OPRED5, &
            TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT, &
            XZ, TAU, TAUZSN )
!
          CALL CALT6( LTAU, INDCHN, LBOT, BLMULT, NCHN6, CLIST6, &
            COEF6, FIXMUL, CONPRD, FPRED6, WPRED6, OPRED6, DPRED, &
            TRCPRD, INDCO2, COFCO2, CO2MLT, INDSO2, COFSO2, SO2MLT, &
            INDN2O, COFN2O, N2OMLT, INDHDO, COFHDO, HDOMLT, &
            XZ, TAU, TAUZSN )
!
          CALL CALT7( LTAU, INDCHN, LBOT, BLMULT, NCHN7, CLIST7, &
            COEF7, FIXMUL, CONPRD, FPRED7, WPRED7, OPRED7, DPRED, &
            TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT, &
            INDHDO, COFHDO, HDOMLT, XZ, TAU, TAUZSN )
!
     ELSE
!       No sun; set the sun surface-to-space trans to zero
        SUNCOS=0.0
        DO I=1,NCHAN
           TAUZSN(I)=0.0
        ENDDO
     ENDIF
  if(DEBUG) write(6,*) 'main: completed caltN after if dosun'
!
!  -----------------------------------------------------
!   Set the emissivity & reflectivity for every channel
!  -----------------------------------------------------
     CALL SETEMS( NCHAN, NEMIS, FREQ, FEMIS, XEMIS, &
         XRHO,LRHOT,  EMIS,RHOSUN,RHOTHR)
!
!       if (DEBUG) print*, 'sarta: completed SETEMS'

!  ------------------------
!   Calculate the radiance
!  ------------------------
     CALL CALRAD ( NCHAN, FREQ, TAU, TEMP, TSURF, EMIS, LBOT, &
         SUNCOS, RHOSUN, HSUN, TAUZSN, &
         SECANG(LBOT), RHOTHR, LABOVE, COEFF, TAUZ, RAD, BT)
!
!    print*, 'sarta: completed CALRAD'
!
     if (COFNTE) then
! -------------------------
!  calculate nonLTE
! -------------------------
! first version call 'calnte.o' for original 90-deg nonLTE
!        CALL CALNTE ( INDCHN, TEMP, SUNCOS, SCOS1, SECANG(1), &
!           NCHNTE, CLISTN, COEFN, CO2TOP, RAD )
! second version call 'calxnte.o' for extended to 120.deg nonLTE
        CALL CALNTE ( INDCHN, TEMP, SECANG(1), NCHNTE, CLISTN, &
         COEFN, CO2TOP, SUNANG, ALT(1), RAD )
 
     endif

!call read_r49_regdata(PREDNTE, DELTAR)

!   -------------------
!   Output the radiance
!   -------------------
     CALL WRTRTP(IPROF, IOPCO, NCHAN, RAD, PROF)
!
!  ----------------------
!  End loop over profiles
!  ----------------------
  if(DEBUG) then
        write(6,2060) IPROF
 2060   FORMAT('sarta: end loop over profiles IPROF: ',I5)
  endif

       IPROF=IPROF + 1  ! increment profile counter
     GOTO 10
!
!  -------------------
!  Close the RTP files
!  -------------------
 9999  if(DEBUG) write(6,*) 'main: closing rtp files'
       ISTAT=rtpclose(IOPCI)
       ISTAT=rtpclose(IOPCO)
!



!IPROF = 1

!call calxnte(ICHAN,FCHAN,IP_YMAX, IP_YMIN, IP_XMAX, IP_XMIN,&
!     b1, b2, IW, LW, OP_YMAX, OP_YMIN, OP_XMAX, OP_XMIN,IPROF,&
!     PREDNTE, DRAD)
!write(6,*) 'main:DRAD(1) = ',DRAD(1)
!
! write one spectrum out to a file
!    OPEN(UNIT=IOUN,FILE='./data/out.txt',FORM='FORMATTED',STATUS='NEW',IOSTAT=IERR)
!    IF (IERR .NE. 0) THEN
!     WRITE(6,1050) IERR, './data/out.txt'
!    STOP
!    ENDIF
! 1050     FORMAT('Error ',I5,' opening output file:',/,A80)

!do ich=1,560
!    write(IOUN,fmt='(8(E11.4,3X))') DRAD(ich)
!enddo

!close(IOUN)


end program main
