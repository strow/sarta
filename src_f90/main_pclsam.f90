!=======================================================================
!    University of Maryland Baltimore County (UMBC)
!    AIRS
!    SARTA_pclsam
!F90====================================================================

!ROUTINE NAME:
!    SARTA_pclsam

!ABSTRACT:
!    Program to quickly compute simulated AIRS radiances.

!    This variant of the code allows for the modelling of a
!    up to two scatter clouds using the PCLSAM method.

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
!    CALOWP  : calc OPTRAN water predictors
!    CALPAR  : calculate a profile's predictors
!    CCPREP  : prepare lookup table for given cp-size.
!    BKPREP  : check cloud layer
!    GETMIE  : determine which scattering lookup table to use
!    GETCLD  : get basic cloud parameters
!    CALRAD0 : calc radiance
!    CALRAD1 : calc radiance
!    CALT1   : calc effective layer trans for set1 (FWO)
!    CALT2   : calc effective layer trans for set2 (FOW)
!    CALT3   : calc effective layer trans for set3 (FMW)
!    CALT4   : calc effective layer trans for set4 (FCOW)
!    CALT5   : calc effective layer trans for set5 (FWO bfsw)
!    CALT6   : calc effective layer trans for set6 (FWO mfmw)
!    CALT7   : calc effective layer trans for set7 (FWO mfbw)
!    FAKETZ  : calc a "fake" (rough approx) surface-to-space trans
!    RDCOEF  : read the fast transmittance coefficients
!    RDPROF  : read profile data
!    RDSUN   : read the solar radiance datafile
!    SUNPAR  : calc a profile's predictors for sets4-7 (sun channels)
!    CALNTE  : calc radiance contribution for non-LTE
!    OPNRTP  : open the RTP file
!    RDRTP   : read the RTP file
!    SETEMS  : sets surface parameters

!FILES ACCESSED:
!    incFTC.f : include file of parameter statements accessed during
!       compilation only.
!    unit IOUN: used by routines RDCOEF and RDPROF.
!    unit 6: USEFAST text messages to the screen
!    unit 5: USEFAST user input instructions, etc
!    unit 10: USEFAST output radiance, text file(s)

!COMMON BLOCKS
!    COMLEV : layer boundary pressure levels

!DESCRIPTION:
!    Dec 2005 version of the SARTA (Stand-Alone Rapid
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
! 29 Mar 2006 Scott Hannon   Add clouds; change TAU from trans to od
! 26 Apr 2006 Scott Hannon   Add black clouds. Redo cloud fractions.
!                            Use RTP v1.06 cloud2 fields instead
!                            of udef(11-17). {Unfinished}
! 22 Dec 2006 Scott Hannon   New & revised code associated with the
!                            new & revised calrad* routines for more
!                            fexible cloud types including black
!                            clouds; changes to calt* calls and
!                            change TAUZ from (1 x n) to (m x n).
! 22 Jan 2007 Scott Hannon   Minor fix of cfrac checks
! 02 May 2007 Scott Hannon   Replace hardcoded default SALT value
!                            with XSALT from incFTC.
! 15 Nov 2007 Scott Hannon   Move most cloud prep to GETCLD & BKPREP.
! 31 Jan 2008 Scott Hannon   Add LCO2PM to allow CO2 profile in ppmv;
!                               add LCO2,LN2O,LSO2,LHNO3 switches
! 24 Mar 2008 Scott Hannon   Add COSDAZ
! 26 Nov 2008 Scott Hannon   Update for rtpV2101
! 01 Dec 2008 Scott Hannon   Add CSTMP1/2
!    Jul 2019 C Hepplewhite  Add NH3 and align with sarta clear updates

!END====================================================================

!      =================================================================
PROGRAM SARTA
!      =================================================================

!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
USE incFTC

!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------
IMPLICIT NONE

INCLUDE 'rtpdefs.f90'

!---------------------------------------------------------------
!      EXTERNAL FUNCTIONS
!---------------------------------------------------------------
real(4) :: VACONV
real(4) :: SACONV
integer :: rtpclose
integer :: rtpopen

!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
!      none (main program)

!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------

! INTEGER, PARAMETER :: IOUN         ! I/O unit number

!      for RDINFO
CHARACTER (LEN=120) :: FIN       ! input RTP filename
CHARACTER (LEN=120) :: FOUT      ! output RTP filename
LOGICAL :: LRHOT         ! force refl therm rho=(1-emis)/pi?
INTEGER :: NWANTP         ! number of wanted profiles (-1=all)
INTEGER :: LISTP(MAXPRO) ! list of wanted profiles

!      for FNMIE
CHARACTER (LEN=240) :: VCLOUD        ! cloud version string
INTEGER :: MIETYP(NMIETY)      ! mie type
CHARACTER (LEN=79) :: FNMIEA(NMIETY) ! mie absorption filenames
CHARACTER (LEN=79) :: FNMIEE(NMIETY) ! mie extinction filenames
CHARACTER (LEN=79) :: FNMIEG(NMIETY) ! mie asymmetry filenames

!      for OPNRTP
INTEGER :: PTYPE         ! profile type
INTEGER :: NCHAN         ! # of selected channels
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
INTEGER :: INH3           ! index of NH3 in gamnt
INTEGER :: IOPCI          ! input RTP unit
INTEGER :: IOPCO          ! output RTP unit
LOGICAL :: LCO2PM         ! CO2 profile in ppmv?
REAL(4) :: FCHAN(MXCHAN) ! chan center frequency

!      for RDCLDT
INTEGER :: MIENPS(NMIETY)            ! number of particle sizes
REAL :: MIEPS(MXMIEA,NMIETY)        ! Mie particle size for table
REAL :: MIEABS(MXCHAN,MXMIEA,NMIETY) ! Mie absorption table
REAL :: MIEEXT(MXCHAN,MXMIEA,NMIETY) ! Mie extinction table
REAL :: MIEASY(MXCHAN,MXMIEA,NMIETY) ! Mie asymmetry table

!      for RDCOEF             ! Info for selected channels only
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
INTEGER :: INDCO2(MXCHAN)            ! chan indices for CO2 pert
REAL :: COFCO2(  NCO2,MAXLAY,MXCHNC) ! coefs for CO2 pert
INTEGER :: INDSO2(MXCHAN)            ! chan indices for SO2 pert
REAL :: COFSO2(  NSO2,MAXLAY,MXCHNS) ! coefs for SO2 pert
INTEGER :: INDHDO(MXCHAN)            ! chan indices for HDO pert
REAL :: COFHDO(  NHDO,MAXLAY,MXCHND) ! coefs for HDO pert
INTEGER :: INDHNO(MXCHAN)            ! chan indices for HNO3 pert
REAL :: COFHNO( NHNO3,MAXLAY,MXCHNH) ! coefs for HNO3 pert
INTEGER :: INDN2O(MXCHAN)            ! chan indices for N2O pert
REAL :: COFN2O(  NN2O,MAXLAY,MXCHNN) ! coefs for N2O pert
INTEGER :: INDNH3(MXCHAN)            ! chan indices for NH3 pert
REAL :: COFNH3(  NNH3,MAXLAY,MXCHNA) ! coefs for NH3 pert
INTEGER :: INDH2O(MXCHAN)            ! chan indices for OPTRAN H2O
REAL :: WAZOP(MXOWLY)              ! OPTRAN water l-to-s amounts
REAL :: WAVGOP(NOWAVG,MXOWLY)       ! OPTRAN raw predictor averages
REAL :: COFH2O(  NH2O,MXOWLY,MXCHNW) ! coefs for OPTRAN H2O
REAL :: FX(MAXLAY)               ! fixed gases adjustment
INTEGER :: NCHNTE                    ! number of non-LTE channels
INTEGER :: CLISTN(MXCNTE)            ! non-LTE channel list
real(4), dimension(XNCOEF,MXCNTE) :: COEFN          ! non-LTE coefficients  

! for rtpopen
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
    REAL :: RAAMNT(MAXLAY) ! ref prof layer ammonia (NH3) amount
    
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
    REAL :: AAMNT(MAXLAY) ! prof layer NH3 amount
    
!      for surface
    INTEGER :: LBOT             ! bottom layer index number
    INTEGER :: NEMIS             ! # of emis pts
    real(4) :: PSURF, BLMULT
    REAL(4),dimension(MXEMIS) :: FEMIS,XEMIS,XRHO  ! emis & reflec freq pts
!   PSURF                ! surface pressure
!   BLMULT                ! bottom layer fractional multiplier
!   FEMIS                 ! emis freq pts
!   XEMIS                 ! emis pts
!   XRHO                  ! reflec pts
    
!      for MEAN_T
    REAL :: TPSEUD(MAXLAY)
    
!      for CALPAR
    LOGICAL :: LCO2             ! CO2 profile switch
    LOGICAL :: LN2O             ! N2O profile switch
    LOGICAL :: LSO2             ! SO2 profile switch
    LOGICAL :: LHNO3             ! HNO3 profile switch
    LOGICAL :: LNH3             ! NH3 profile switch
    LOGICAL :: LHDO             ! HDO profile switch
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
    REAL :: DPRED(  NHDO,MAXLAY) ! HDO perturbation predictors
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
    REAL :: NH3MLT(MAXLAY)        ! NH3 perturbation multiplier
    REAL :: HDOMLT(MAXLAY)        ! HDO perturbation multiplier
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
    REAL :: TAU(MAXLAY,MXCHAN) ! chan layer effective optical depth
    REAL :: TAUZ(MAXLAY,MXCHAN) ! chan surface-to-space trans
    REAL :: WAOP(MXOWLY)        ! OPTRAN abs coef scaling factor
    REAL :: XZ                 ! Optical depth multiplier for TAUZ
    LOGICAL :: LTAU              ! calc all layer transmittances?
          
!      for SETEMS
    REAL :: EMIS(MXCHAN) ! chan surface emissivity
    REAL :: RHOSUN(MXCHAN) ! chan reflectivity for sun
    REAL :: RHOTHR(MXCHAN) ! chan reflectivity for downwelling thermal
    REAL :: CEMIS1(MXCHAN) ! chan surface emissivity cloud1
    REAL :: CRHOS1(MXCHAN) ! chan solar reflectivity cloud1
    REAL :: CRHOT1(MXCHAN) ! chan thermal reflectivity cloud1
    REAL :: CEMIS2(MXCHAN) ! chan surface emissivity cloud2
    REAL :: CRHOS2(MXCHAN) ! chan solar reflectivity cloud2
    REAL :: CRHOT2(MXCHAN) ! chan thermal reflectivity cloud2
         
!      for CALRAD(0,1)
    REAL :: SUNFAC         ! sun solid angles times cosine at surface
    REAL :: RPLNCK(MAXLAY) ! layer Planck
    REAL :: RSURFE         ! surface emission
    REAL :: RSURFC         ! black cloud surface emission
    REAL :: TRANL(MAXLAY) ! clear air layer transmittance
    REAL :: TRANZ(MXCHAN) ! clear air layer-to-space transmittance
    REAL :: TRANS(MXCHAN) ! clear air total reflected solar trans
    REAL :: TSURF         ! surface temperature
    REAL :: RAD(MXCHAN) ! chan radiance
!      For clear/cloudy radiances
      REAL :: RAD0         ! radiance no clouds
      REAL :: RADC1         ! radiance cloud1
      REAL :: RADC2         ! radiance cloud2
      REAL :: RADC12         ! radiance cloud1+cloud2
          
!      for RDSUN
      REAL :: HSUN(MXCHAN) ! sun radiance (direct from sun)
!      Other variables for the sun
      REAL :: SUNANG         ! solar zenith angle (at 0 altitude)
      REAL :: COSDAZ         ! cosine(solazi - satazi) {COS Delta AZimuth}
      REAL :: SZALAY         ! solar zenith angle in some layer
      REAL :: SUNCOS         ! cosine of sun zenith angle
      REAL :: SCOS1          ! cosine of sun zenith angle at layer1
      REAL :: SUNFDG         ! fudge factor for large solar angles
      REAL :: SECSUN(MAXLAY) ! secant of effective sun local path angle
!     REAL :: DISTES         ! distance of Earth from the sun
      REAL :: TAUZSN(MAXLAY,MXCHAN) ! sun space-to-surface-to-space OD
      LOGICAL :: DOSUN       ! do sun calc?
          
!      for satellite viewing angle
       REAL :: SATANG      ! input satellite scan angle (degrees)
       REAL :: SATZEN      ! input satellite zenith angle (degrees)
       REAL :: SALT        ! input satellite altitude (kilometers)
       REAL :: SVA         ! satellite viewing angle (degrees)
          
!      for RDRTP
       INTEGER :: IPROF      ! profile loop counter
       LOGICAL :: LWANT      ! do you want this profile?
       real(4) :: HDODPL         ! HDO depletion from prof.udef(20,:)
          
!      Basic cloud info
        REAL :: XCEMI1(MXEMIS)    ! cloud1 emissivity
        REAL :: XCEMI2(MXEMIS)    ! cloud2 emissivity
        REAL :: XCRHO1(MXEMIS)    ! cloud1 reflectivity
        REAL :: XCRHO2(MXEMIS)    ! cloud2 reflectivity
        REAL :: CFRAC1            ! cloud1(total) fraction of FOV
        REAL :: CFRAC2            ! cloud2(total) fraction of FOV
        REAL :: CFRA1X            ! cloud1(exclusively) fraction of FOV
        REAL :: CFRA2X            ! cloud2(exclusively) fraction of FOV
        REAL :: CFRA12            ! cloud1+2(both) fraction of FOV
        REAL :: CNGWA1            ! cloud1 non-gases water
        REAL :: CNGWA2            ! cloud1 non-gases water
        REAL :: CPRBO1            ! cloud1 bottom pressure
        REAL :: CPRBO2            ! cloud2 bottom pressure
        REAL :: CPRTO1            ! cloud1 top pressure
        REAL :: CPRTO2            ! cloud2 top pressure
        REAL :: CPSIZ1            ! cloud1 particle size
        REAL :: CPSIZ2            ! cloud2 particle size
        REAL :: CSTMP1            ! cloud1 top/surf temperature
        REAL :: CSTMP2            ! cloud2 top/surf temperature
        REAL :: FCLEAR            ! clear (no cloud) fraction of FOV
        REAL :: TEMPC1            ! cloud1 frac layer (above cloud) mean temp
        REAL :: TEMPC2            ! cloud2 frac layer (above cloud) mean temp
        INTEGER :: CTYPE1         ! cloud1 type code number
        INTEGER :: CTYPE2         ! cloud2 type code number
          
!      for GETMIE
        LOGICAL :: LBLAC1  ! black cloud1? {Mie cloud if false}
        LOGICAL :: LBLAC2  ! black cloud2? {Mie cloud if false}
        INTEGER :: INDMI1  ! index in MIETYP for CTYPE1
        INTEGER :: INDMI2  ! index in MIETYP for CTYPE2
        INTEGER :: IERR1  ! error level of CTYPE1/MIETYP match
        INTEGER :: IERR2  ! error level of CTYPE2/MIETYP match
          
!      for CCPREP cloud1
        INTEGER :: LCBOT1         ! layer containing cloud bottom
        INTEGER :: LCTOP1         ! layer containing cloud top
        REAL :: CLRB1            ! frac of layer at bottom of cloud clear
        REAL :: CLRT1            ! frac of layer at top of cloud clear
        REAL :: TCBOT1            ! temperature at cloud bottom
        REAL :: TCTOP1            ! temperature at cloud top
        REAL :: MASEC1            ! mean cloud view angle secant
        REAL :: MASUN1            ! mean cloud sun-only angle secant
        REAL :: CFRCL1(MAXLAY)    ! fraction of cloud in layer
        REAL :: G_ASY1(MXCHAN)    ! "g" asymmetry
        REAL :: NEXTO1(MXCHAN)    ! nadir extinction optical depth
        REAL :: NSCAO1(MXCHAN)    ! nadir scattering optical depth
          
!      for CCPREP cloud2
        INTEGER :: LCBOT2         ! layer containing cloud bottom
        INTEGER :: LCTOP2         ! layer containing cloud top
        REAL :: CLRB2            ! frac of layer at bottom of cloud clear
        REAL :: CLRT2            ! frac of layer at top of cloud clear
        REAL :: TCBOT2            ! temperature at cloud bottom
        REAL :: TCTOP2            ! temperature at cloud top
        REAL :: MASEC2            ! mean cloud view angle secant
        REAL :: MASUN2            ! mean cloud sun-only angle secant
        REAL :: CFRCL2(MAXLAY)    ! fraction of cloud in layer
        REAL :: G_ASY2(MXCHAN)    ! "g" asymmetry
        REAL :: NEXTO2(MXCHAN)    ! nadir extinction optical depth
        REAL :: NSCAO2(MXCHAN)    ! nadir scattering optical depth
          
!      used locally only
        INTEGER :: I      ! loop counter
        INTEGER :: L      ! loop counter
        INTEGER :: ICLOUD   ! cloud type for BKPREP
        INTEGER :: ICH      ! channel index for calrad0,1
        REAL :: EVA         ! (Earth) local viewing angle
        REAL :: CONV         ! degrees to radians conversion factor
        REAL :: ANGMAX         ! maximum allowed viewing angle
        REAL :: RJUNK1         ! junk/work
        REAL :: RJUNK2         ! another junk/work
        REAL(4) :: CO2PPM         ! Profile mean dry air CO2 mixing ratio
        REAL :: PLAY(MAXLAY)   ! layer mean pressure
        REAL :: C1V3           ! rad constant c1 times freq^3
        REAL :: C2V            ! rad constant c2 times freq
        REAL :: VSTORE(6)      ! temporary storage for various variables
          
!      Profile data structure
        INTEGER :: ISTAT
        RECORD /RTPPROF/ PROF            ! profile
        RECORD /RTPHEAD/ HEAD            ! header data
        RECORD /RTPATTR/ HATT(MAXNATTR)  ! header attributes
        RECORD /RTPATTR/ PATT(MAXNATTR)  ! profile attributes
          
!      Boundary pressure levels
        COMMON /COMLEV/ PLEV
        REAL(4), dimension(MAXLAY+1) :: PLEV
          
!      for function QIKEXP
        REAL :: QIKEXP
          
! ----------------------------------------
! for calxnte_nn
! ----------------------------------------
      real(4) :: XALT, VSEC1
      real(4), dimension(MXCHAN) :: DRAD

      if(DEBUG) print*,'main: completed initialization'
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
          
!      --------------------------
!      Assign the I/O unit number
!      --------------------------
!       IOUN=11
          
!      ---------
!      Calc PLAY
!      ---------
!      Mean layer pressure (KLAYERS definition)
       DO L=1,MAXLAY
         PLAY(L) = ( PLEV(L+1) - PLEV(L) )/LOG( PLEV(L+1)/PLEV(L) )
       ENDDO
              
!  -----------------------------
!  Read in the reference profile
!  -----------------------------
CALL rdpref(IOUN, FNPREF, RPNAM, RALT, RDZ, RPRES, RTEMP, &
         RFAMNT, RWAMNT, ROAMNT, RCAMNT, RMAMNT, RSAMNT, &
         RHAMNT, RNAMNT, RAAMNT)

if (DEBUG) print*, 'sarta: completed call rdprof'

!      ---------------------
!      Get command-line info
!      ---------------------
       CALL RDINFO(FIN, FOUT, LRHOT, NWANTP, LISTP)
!
       IF (DEBUG) THEN
         PRINT *, 'nwantp=', NWANTP
         PRINT *, 'listp=', (LISTP(I),I=1,NWANTP)
         PRINT *, 'FIN = ',FIN
         PRINT *, 'FOUT = ',FOUT
       END IF
!cc
              
!      -------------------------
!      Get cloud table filenames
!      -------------------------
       CALL FNMIE ( VCLOUD, MIETYP, FNMIEA, FNMIEE, FNMIEG )
              
!      ---------------------------
!      Open & check input RTP file
!      ---------------------------
       CALL OPNRTP(FIN, VCLOUD, LRHOT, PTYPE, NCHAN, FCHAN, LSTCHN,  &
           INDCHN, IH2O, IO3, ICO, ICH4, ICO2, ISO2, IHNO3, IN2O,  &
           INH3, IOPCI, HEAD, HATT, PATT, LCO2PM)
              
!       print*, 'sarta_cloudy: completed opnrtp'
!cc
!      ------------------------
!      Read cloud lookup tables
!      ------------------------
       CALL RDCLDT( IOUN, INDCHN, MIETYP, FNMIEA, FNMIEE, FNMIEG,  &
           MIENPS, MIEPS, MIEABS, MIEEXT, MIEASY )
              
!      ------------------------
!      Read the coef data files
!      ------------------------
       CALL RDCOEF( IOUN, NCHAN, INDCHN, SETCHN, &
         NCHN1,  NCHN2,  NCHN3,  NCHN4,  NCHN5,  NCHN6,  NCHN7, &
         CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7, &
         COEF1,  COEF2,  COEF3,  COEF4,  COEF5,  COEF6,  COEF7, &
         FREQ, LABOVE,  COEFF, INDCO2, COFCO2, INDSO2, COFSO2, &
         INDHNO, COFHNO, INDN2O, COFN2O, INDNH3, COFNH3, INDHDO, &
         COFHDO, INDH2O,  WAZOP, WAVGOP, COFH2O, FX, NCHNTE, CLISTN, COEFN )
                
!      Get and apply multipler tuning to coefficients {note: ignores HNO3}
       call tunmlt(IOUN, NCHAN, INDCHN, SETCHN, &
          NCHN1,  NCHN2,  NCHN3,  NCHN4,  NCHN5,  NCHN6,  NCHN7, &
         CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7, &
          COEF1,  COEF2,  COEF3,  COEF4,  COEF5,  COEF6,  COEF7, &
           FREQ, LABOVE, COEFF,  INDCO2, COFCO2, INDSO2, COFSO2, &
         INDHNO, COFHNO, INDN2O, COFN2O, &
         INDH2O,  WAZOP, WAVGOP, COFH2O, FX)  ! , NCHNTE, CLISTN, COEFN )
                
!      Calc OPTRAN absorption coefficient scaling factor WAOP
       WAOP(1)=WAZOP(1)
       DO L=2,MXOWLY
         WAOP(L)=WAZOP(L) - WAZOP(L-1)
       ENDDO
                    
!      --------------------------
!      Read in the solar radiance
!      --------------------------
       CALL RDSUN(IOUN, INDCHN, HSUN)
                   
!      --------------------
!      Check FREQ and FCHAN
!      --------------------
!      Note: FREQ comes the coef data, while FCHAN comes
!      from the input RTP file read by OPNRTP.  It is possible
!      that FCHAN is "nodata", so we check the first element.
       IF (FCHAN(1) > 640.0 .AND. FCHAN(1) < 2670.0) THEN
         DO I=1,NCHAN
           RJUNK1=ABS(FREQ(I) - FCHAN(I))
           RJUNK2=0.01*FREQ(I)/1200.0   ! ~1% of a channel fullwidth
           IF (RJUNK1 > RJUNK2) THEN
              WRITE(IOINFO,1010) I, LSTCHN(I), FREQ(I), FCHAN(I)
 1010         FORMAT('Warning! index=',I4,', chan ID=',I4,  &
               ', fastmodel freq=',F8.3,', RTP freq=',F8.3)
           END IF
           HEAD%vchan(I)=FREQ(I)
         ENDDO
       ELSE
         DO I=1,NCHAN
           HEAD%vchan(I)=FREQ(I)
         ENDDO
       END IF
                            
!      ------------------------
!      Open the output RTP file
!      ------------------------
       MODE='c'
       ISTAT=rtpopen(FOUT, MODE, HEAD, HATT, PATT, IOPCO)
                            
!cc
       IF (DEBUG)  PRINT *, 'read open status = ', ISTAT
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
!       Look for current profile on list of wanted profiles
          LWANT=.FALSE.
          DO I=1,NWANTP
            IF (IPROF == LISTP(I)) LWANT=.TRUE.
          ENDDO
       END IF

       print*, '=========================================================='             
       print *, 'main_pclsam: iprof=', IPROF

!      --------------
!      Read input RTP
!      --------------
       CALL RDRTP( LWANT, IPROF, IOPCI,  &
           IH2O, IO3, ICO, ICH4, ICO2, ISO2, IHNO3, IN2O, INH3, PTYPE,  &
           RALT, LCO2PM, NLAY, NEMIS, LAT, LON, SATANG, SATZEN,  &
           SALT, SUNANG, COSDAZ, PSURF, TSURF, CO2PPM, HDODPL,  &
           FEMIS, XEMIS, XRHO,  &
           TEMP, WAMNT, OAMNT, CAMNT, MAMNT, FAMNT, SAMNT, HAMNT,  &
           NAMNT, AAMNT, ALT, PROF, ISTAT )
                                            
       IF (ISTAT == -1) GO TO 9999  ! reached End Of File
                                          
       IF (.NOT. LWANT) THEN
!         Skip this profile
          IPROF=IPROF+ 1
          GO TO 10
        END IF
                                            
! ------------------------------------------
!     check prof.plevs match COMLEV plev
! ------------------------------------------
       CALL CHECK_PLEVS(PROF, NLAY)

!      -------------------------------------
!      Determine bottom layer, CO2, & angles
!      -------------------------------------
       CALL GETBOT(NLAY, PLEV, PSURF, LBOT, BLMULT)
                                            
  if(DEBUG) print*, 'main: completed getbot, LBOT,BLMULT', &
         LBOT,BLMULT
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
!      NH3 profile switch
       IF (INH3 < 1) THEN
         LNH3=.FALSE.
       ELSE
         LNH3=.TRUE.
       END IF
!      HNO3 profile switch
       IF (IHNO3 < 1) THEN
         LHNO3=.FALSE.
       ELSE
         LHNO3=.TRUE.
       END IF
!      HDO switch (default .TRUE. from water)
       LHDO=.FALSE.
                                            
       IF (PTYPE == AIRSLAY) THEN
!         Copy pseudo level temperatures to another array
          DO I=1,LBOT
             TPSEUD(I)=TEMP(I)
          ENDDO
!         Convert temperatures
          CALL MEAN_T(LBOT, PLEV, PSURF, TPSEUD, TEMP)
                                                  
       ELSE
!       Calc mean pressure for bottom fractional layer
         RJUNK1 = ( PSURF - PLEV(LBOT) )/LOG( PSURF/PLEV(LBOT) )
!         Do interpolation for fractional bottom layer mean temperature
!         assuming T is in linear in log(P)
         RJUNK2=( TEMP(LBOT) - TEMP(LBOT-1) )/  &
            LOG( PLAY(LBOT)/PLAY(LBOT-1) )             ! slope
         TEMP(LBOT)=RJUNK2*LOG( RJUNK1/PLAY(LBOT-1) ) + TEMP(LBOT - 1)
       END IF

! ---------------------------------------                                                
!      Check satellite elevation
! ---------------------------------------
       IF (SALT > 0.0) THEN
!         Warn and use default if invalid
          IF (SALT < XSALT-50 .OR. SALT > XSALT+50) THEN
              WRITE(IOINFO,1020) IPROF, SALT, XSALT
 1020         FORMAT('Warning! Profile',I5,  &
           ': replacing invalid input satellite altitude ',  &
           1PE11.4,' with default ',1PE11.4,' km')
           SALT=XSALT
           END IF
        ELSE
           SALT=XSALT
        END IF

! --------------------------------------------------                                          
!      Convert SATZEN or SATANG to viewing angle
! -------------------------------------------------
        IF (SATZEN >= 0.0 .AND. SATZEN < 63.0) THEN
!       Convert zenith angle at surface to view angle at satellite
           SVA=SACONV( SATZEN, SALT*1000.0 )/CONV
        ELSE
!        Check if scan angle is valid
          IF (SATANG > -49.6 .AND. SATANG < 49.6) THEN
!           View angle should be within a few degrees of scan angle
             SVA=ABS( SATANG )
          ELSE
            WRITE(IOERR,1030) IPROF, SATZEN, SATANG
 1030        FORMAT('Error! Profile',I5,  &
            ': invalid angles for SATZEN ',1PE11.4,  &
            ' and SATANG ',E11.4)
            STOP
          END IF
       END IF
                                                
! --------------------------
! truncate view angle
! --------------------------
       ANGMAX=53  ! max satellite view angle (49.5 scan + 3.5 spacecraft)
       IF (SVA > ANGMAX) THEN
!         Truncate angle if too big
          WRITE(IOINFO,1040) IPROF, SVA
 1040     FORMAT('Warning! Profile',I5,': truncating view angle ',  &
            1PE11.4,' to 53 degrees')
          SVA=ANGMAX
       END IF
! --------------------------------------------------------------- 
!      Convert from satellite to earth viewing angle (in radians)
! ---------------------------------------------------------------
       DO L=1,LBOT
          EVA=VACONV(SVA, SALT, ALT(L))
          SECANG(L)=1.0E+0/COS(EVA)
!cccccccccccc
!            for testing
!             SECANG(L)=SVA
!cccccccccccc
       ENDDO
! ----------------------------------------
!      Calc total sun angle secant
! ----------------------------------------
       DOSUN=.FALSE.
       IF (SUNANG >= 0.0 .AND. SUNANG < 89.9) DOSUN=.TRUE.
       IF (DOSUN) THEN
         SUNCOS=COS(CONV*SUNANG)
         SZALAY=SACONV(SUNANG,ALT(1))
         SCOS1=COS(SZALAY)
         RJUNK2=SECANG(LBOT) + 1.0/SUNCOS ! Total secant
       
!         Calc non-unity fudge factor if total secant > 9
         IF (RJUNK2 > 9.0) THEN
!           fudge factor = true_total_secant/calc_total_secant
            SUNFDG=RJUNK2/9.0
!            truncated solar angle to use to calc SECSUN
             RJUNK1=ACOS( 1.0/(9.0 - SECANG(LBOT)) )/CONV
         ELSE
           SUNFDG=1.0
           RJUNK1=SUNANG
         END IF
! Should I change SUNFDG to SUNFDG(MAXLAY)?
     
         DO L=1,LBOT
           SZALAY=SACONV(RJUNK1,ALT(L))
           SECSUN(L)=SECANG(L) + 1.0E+0/COS(SZALAY)
         ENDDO
       END IF
      
!      -----------------------------------
!      Calculate the fast trans predictors
!      -----------------------------------
   
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
!      -----------------------------------
!      Calculate the OPTRAN H2O predictors
!      -----------------------------------
      IF (CFOPTR) THEN
         CALL CALOWP ( LBOT, WAMNT, RPRES, TEMP, SECANG, WAZOP, WAVGOP,  &
            WAANG, LOPMIN, LOPMAX, LOPUSE, H2OPRD, LOPLOW, DAOP )
      ENDIF
!      ----------------------------------
!      Calculate the layer transmittances
!      ----------------------------------
!      Calculate TAU for set 1 thru 7
     
       CALL XCALT1( INDCHN,  LBOT,   NCHN1, CLIST1,  COEF1,  &
       FIXMUL, CONPRD, FPRED1, WPRED1, DPRED, OPRED1, TRCPRD,  &
       INDCO2, COFCO2, CO2MLT, INDSO2, COFSO2, SO2MLT,  &
       INDHNO, COFHNO, HNOMLT, INDN2O, COFN2O, N2OMLT,  &
       INDNH3, COFNH3, NH3MLT, INDHDO, COFHDO, HDOMLT,  &
       INDH2O, H2OPRD, COFH2O, LOPMIN, LOPMAX, LOPLOW,  &
       LOPUSE,   WAOP,   DAOP, WAANG,     TAU,   TAUZ)
      
       CALL XCALT2( INDCHN, LBOT,   NCHN2, CLIST2,  COEF2,  &
       FIXMUL, CONPRD, FPRED2, OPRED2, WPRED2, DPRED, TRCPRD,  &
       INDCO2, COFCO2, CO2MLT, INDSO2, COFSO2, SO2MLT,  &
       INDHNO, COFHNO, HNOMLT, INDN2O, COFN2O, N2OMLT,  &
       INDNH3, COFNH3, NH3MLT, INDHDO, COFHDO, HDOMLT,TAU, TAUZ)
      
       CALL XCALT3( INDCHN,   LBOT,  NCHN3, CLIST3,  COEF3,  &
       FIXMUL, CONPRD, FPRED3, MPRED3, WPRED3, DPRED, TRCPRD,  &
       INDSO2, COFSO2, SO2MLT, INDHNO, COFHNO, HNOMLT,  &
       INDN2O, COFN2O, N2OMLT, INDNH3, COFNH3, NH3MLT,  &
       INDHDO, COFHDO, HDOMLT, INDH2O, H2OPRD, COFH2O,  &
       LOPMIN, LOPMAX, LOPLOW, LOPUSE,  &
       WAOP,   DAOP,  WAANG,    TAU,   TAUZ)
       
       CALL XCALT4(INDCHN,   LBOT,  NCHN4, CLIST4,  &
       COEF4, FIXMUL, CONPRD, FPRED4, CPRED4, OPRED4, WPRED4,  &
       TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT,  &
       TAU,   TAUZ )
       
       CALL XCALT5(INDCHN,   LBOT,  NCHN5, CLIST5,  &
       COEF5, FIXMUL, CONPRD, FPRED5, WPRED5, OPRED5,  &
       TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT,  &
       TAU,   TAUZ )
       
       CALL XCALT6(INDCHN,   LBOT,  NCHN6, CLIST6,  &
       COEF6, FIXMUL, CONPRD, FPRED6, WPRED6, OPRED6, DPRED, TRCPRD,  &
       INDCO2, COFCO2, CO2MLT, INDSO2, COFSO2, SO2MLT,  &
       INDN2O, COFN2O, N2OMLT, INDHDO, COFHDO, HDOMLT,  TAU,  TAUZ )
       
       CALL XCALT7(INDCHN,   LBOT,  NCHN7, CLIST7,  &
       COEF7, FIXMUL, CONPRD, FPRED7, WPRED7, OPRED7,  &
       TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT,  &
       TAU,   TAUZ )

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
         CALL FAKETZ( NFAKE, INDFAK, LBOT, TAUZ, SECANG,  &
         SECSUN, TAUZSN)
 
!         Calculate TAUZSN for sets 4 thru 7
      
        CALL XCALT4(INDCHN,   LBOT,  NCHN4, CLIST4,  &
        COEF4, FIXMUL, CONPRD, FPRED4, CPRED4, OPRED4, WPRED4,  &
        TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT,  &
        TAUZSN, TAUZSN )
!         ^^^    ^^^
!        dummy   actual
        CALL XCALT5(INDCHN,   LBOT,  NCHN5, CLIST5,  &
         COEF5, FIXMUL, CONPRD, FPRED5, WPRED5, OPRED5,  &
         TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT,  &
         TAUZSN, TAUZSN )
         
        CALL XCALT6(INDCHN,   LBOT,  NCHN6, CLIST6,  &
         COEF6, FIXMUL, CONPRD, FPRED6, WPRED6, OPRED6, DPRED,  &
         TRCPRD, INDCO2, COFCO2, CO2MLT, INDSO2, COFSO2, SO2MLT,  &
         INDN2O, COFN2O, N2OMLT, INDHDO, COFHDO, HDOMLT, TAUZSN, TAUZSN )
         
        CALL XCALT7(INDCHN,   LBOT,  NCHN7, CLIST7,  &
          COEF7, FIXMUL, CONPRD, FPRED7, WPRED7, OPRED7,  &
          TRCPRD, INDCO2, COFCO2, CO2MLT, INDN2O, COFN2O, N2OMLT,  &
          TAUZSN, TAUZSN )
          
         IF (SUNFDG > 1.0001) THEN
           DO I=1,NCHAN
           DO L=1,LBOT
              TAUZSN(L,I)=TAUZSN(L,I)*SUNFDG
           ENDDO
        ENDDO
        END IF
        
       ELSE
!      No sun; set the sun surface-to-space trans to zero
         SUNCOS=0.0
         DO I=1,NCHAN
            DO L=1,LBOT
               TAUZSN(L,I)=0.0
            ENDDO
        ENDDO
      END IF
      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!      Calculate cloudy radiance

!      Get basic cloud parameters from input RTP
      CALL GETCLD( IPROF, HEAD, PROF,  &
        LBLAC1, CTYPE1, CFRAC1, CPSIZ1, CPRTO1, CPRBO1, CNGWA1,  &
        XCEMI1, XCRHO1, CSTMP1,  &
        LBLAC2, CTYPE2, CFRAC2, CPSIZ2, CPRTO2, CPRBO2, CNGWA2,  &
        XCEMI2, XCRHO2, CSTMP2, CFRA12, FCLEAR, CFRA1X, CFRA2X )
      IF (DEBUG) THEN
        PRINT *,'getcld ',IPROF,CTYPE1, CFRAC1, CPSIZ1, CPRTO1,  &
          CPRBO1, CNGWA1,CFRA1X
      END IF
!      ---------------------------------------------------
!      Set the emissivity & reflectivity for every channel
!      ---------------------------------------------------
      CALL SETEMS( NCHAN, NEMIS, FREQ, FEMIS, XEMIS, XRHO,  &
        XCEMI1, XCRHO1, XCEMI2, XCRHO2, LRHOT,  &
        EMIS, RHOSUN, RHOTHR, CEMIS1, CRHOS1, CRHOT1,  &
        CEMIS2, CRHOS2, CRHOT2)
        
!       print *,CFRAC1,CFRAC2,CFRA12,LBLAC1,LBLAC2
!!!!!! force zero for over-ride !!!!!!
!       CFRAC1 = 0.0
!       CFRA1X = 0.0
!       CFRA2X = 0.0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      Check and prepare (top) cloud1
       IF (CFRAC1 > 0.0) THEN
         IF (LBLAC1) THEN
           ICLOUD = 1
           CALL BKPREP(IPROF,ICLOUD, CTYPE1, CFRAC1, CPRTO1,  &
            LBOT, PSURF, PLEV, PLAY, TEMP, LCTOP1, TCTOP1,  &
            TEMPC1, CLRT1)
         IF (CSTMP1 > 0.0) TCTOP1=CSTMP1
         ELSE
!        Determine which lookup table to use
           CALL GETMIE(CTYPE1,MIETYP,INDMI1,IERR1)
!          Prepare selected lookup table for given cpsize
           CALL CCPREP( NCHAN, LBOT, INDMI1, MIENPS,  &
             CNGWA1, CPSIZ1, CPRTO1, CPRBO1, PLEV, TEMP, SECANG,  &
             SECSUN, MIEPS, MIEABS, MIEEXT, MIEASY, LCBOT1, LCTOP1,  &
             CLRB1, CLRT1, TCBOT1, TCTOP1, MASEC1, MASUN1,  &
             CFRCL1, G_ASY1, NEXTO1, NSCAO1 )
          END IF
       END IF

!!!!!!! force CFRAC2 = 0 over-ride !!!!!!!!!!
!      CFRAC2 = 0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!       
!      Check and prepare (bottom) cloud2
      IF (CFRAC2 > 0.0) THEN
         IF (LBLAC2) THEN
           ICLOUD = 2
           CALL BKPREP(IPROF, ICLOUD, CTYPE2, CFRAC2, CPRTO2,  &
              LBOT, PSURF, PLEV, PLAY, TEMP, LCTOP2, TCTOP2,  &
              TEMPC2, CLRT2)
         IF (CSTMP2 > 0.0) TCTOP2=CSTMP2
         ELSE
!     Determine which lookup table to use
            CALL GETMIE(CTYPE2,MIETYP,INDMI2,IERR2)
!     Prepare lookup data for cloud2
            CALL CCPREP( NCHAN, LBOT, INDMI2, MIENPS,  &
              CNGWA2, CPSIZ2, CPRTO2, CPRBO2, PLEV, TEMP, SECANG,  &
              SECSUN, MIEPS, MIEABS, MIEEXT, MIEASY, LCBOT2, LCTOP2,  &
              CLRB2, CLRT2, TCBOT2, TCTOP2, MASEC2, MASUN2,  &
              CFRCL2, G_ASY2, NEXTO2, NSCAO2 )
         END IF
      ELSE
!     Safe default for non-existant cloud2
         LCTOP2=1
      END IF
      
!ccccccc this block for testing only
!      PROF%udef(19)=TCTOP1
!      PROF%udef(20)=TCTOP2
!ccccccccccccccccccccccccccccccccccc
      
      SUNFAC=SUNCOS*PI*(RADSUN/DISTES)**2
!      Note: PI*(RADSUN/DISTES)^2 = solid angle [steradians] of
!      the sun as seen from Earth for the case DISTES >> RADSUN.
      
!      ----------------------
!      Loop over the channels
!      ----------------------
      DO I=1,NCHAN
     
!     Radiation constants for current channel
         C1V3=C1*(FREQ(I)**3)
         C2V=C2*FREQ(I)
      
!      Calculate Planck & clear airs trans for full layers
          DO L=1,LBOT-1
             RPLNCK(L)=C1V3/( EXP( C2V/TEMP(L) ) - 1.0 )
             TRANL(L)=QIKEXP( -TAU(L,I) )
          ENDDO
!      Note: TEMP(LBOT) already adjusted for bottom fractional layer
          RPLNCK(LBOT)=C1V3/( EXP( C2V/TEMP(LBOT) ) - 1.0 )
       
!      Calculate clear airs trans for bottom fractional layer
          RJUNK1=-TAU(LBOT,I)*BLMULT
          TRANL(LBOT)=QIKEXP( RJUNK1 )
          TRANL(LBOT)=QIKEXP( RJUNK1 )
          TRANZ(I)=QIKEXP( RJUNK1 - TAUZ(LBOT-1,I) )
          TRANS(I)=QIKEXP( BLMULT*(TAUZSN(LBOT-1,I)-TAUZSN(LBOT,I)) -  &
             TAUZSN(LBOT-1,I) )
       
!       Planck for surface
          RSURFE=EMIS(I)*C1V3/( EXP( C2V/TSURF ) - 1.0 )
       
!       Calculate clear radiance
          IF (FCLEAR > 0.0) THEN
            ICH = I
            CALL CALRAD0( DOSUN, ICH, LBOT, RPLNCK, RSURFE, SECANG,  &
               TRANL, TRANZ, SUNFAC, HSUN, TRANS, RHOSUN,  &
               RHOTHR, LABOVE, COEFF, RAD0 )
          ELSE
             RAD0=0.0
          END IF
       
!       Store original values
          VSTORE(1)=TRANL(LCTOP2)
          VSTORE(2)=TRANZ(I)
          VSTORE(3)=TRANS(I)
          VSTORE(4)=RHOTHR(I)
          VSTORE(5)=RHOSUN(I)
          VSTORE(6)=RPLNCK(LCTOP2)
!       Updates for new surface if bottom cloud2 is black
          IF (CFRAC2 > 0.0 .AND. LBLAC2) THEN
              RJUNK1=-TAU(LCTOP2,I)*CLRT2
              TRANL(LCTOP2)=QIKEXP( RJUNK1 )
              TRANZ(I)=QIKEXP( RJUNK1 - TAUZ(LCTOP2-1,I) )
              TRANS(I)=QIKEXP( CLRT2*(TAUZSN(LCTOP2-1,I)-  &
                 TAUZSN(LCTOP2,I)) - TAUZSN(LCTOP2-1,I) )
              RSURFC=CEMIS2(I)*C1V3/( EXP( C2V/TCTOP2 ) - 1.0 )
              RHOTHR(I)=CRHOT2(I)
              RHOSUN(I)=CRHOS2(I)
              RPLNCK(LCTOP2)=C1V3/( EXP( C2V/TEMPC2 ) - 1.0 )
!              RSURFC=C1V3/( EXP( C2V/TEMPC2 ) - 1.0 )
          END IF
        
!       Calculate bottom cloud2 radiance
          IF (CFRA2X > 0.0) THEN
             ICH = I
             IF (LBLAC2) THEN
               CALL CALRAD0( DOSUN, ICH, LCTOP2, RPLNCK, RSURFC, SECANG,  &
                 TRANL, TRANZ, SUNFAC, HSUN, TRANS, RHOSUN,  &
                 RHOTHR, LABOVE, COEFF, RADC2 )
             ELSE
               CALL CALRAD1( DOSUN, ICH, LBOT, RPLNCK, RSURFE, SECANG,  &
                  TAU, TRANL, TRANZ, SUNFAC, HSUN, TRANS, RHOSUN,  &
                  RHOTHR, LABOVE, COEFF, CFRCL2, MASEC2, MASUN2, COSDAZ,  &
                  NEXTO2, NSCAO2, G_ASY2, LCTOP2, LCBOT2, RADC2 )
             END IF
          ELSE
            RADC2=0.0
          END IF
         
!         Calculate combined cloud1+cloud2 radiance
          IF (CFRA12 > 0.0) THEN
             ICH = I
             IF (LBLAC2) THEN
               CALL CALRAD1( DOSUN, ICH, LCTOP2, RPLNCK, RSURFC, SECANG,  &
                  TAU, TRANL, TRANZ, SUNFAC, HSUN, TRANS, RHOSUN,  &
                  RHOTHR, LABOVE, COEFF, CFRCL1, MASEC1, MASUN1, COSDAZ,  &
                  NEXTO1, NSCAO1, G_ASY1, LCTOP1, LCBOT1, RADC12 )
             ELSE
                CALL CALRAD2( DOSUN, ICH, LBOT, RPLNCK, RSURFE, SECANG,  &
                  TAU, TRANL, TRANZ, SUNFAC, HSUN, TRANS, RHOSUN,  &
                  RHOTHR, LABOVE, COEFF, CFRCL1, MASEC1, MASUN1, NEXTO1,  &
                  NSCAO1, G_ASY1, LCTOP1, LCBOT1, CFRCL2, MASEC2, MASUN2,  &
                  COSDAZ, NEXTO2, NSCAO2, G_ASY2, LCTOP2, LCBOT2, RADC12 )
             END IF
          ELSE
             RADC12=0.0
          END IF
           
!         Restore original values
          TRANL(LCTOP2)=VSTORE(1)
          TRANZ(I)=VSTORE(2)
          TRANS(I)=VSTORE(3)
          RHOTHR(I)=VSTORE(4)
          RHOSUN(I)=VSTORE(5)
          RPLNCK(LCTOP2)=VSTORE(6)
!         Updates for new surface if top cloud1 is black
          IF (CFRAC1 > 0.0 .AND. LBLAC1) THEN
             RJUNK1=-TAU(LCTOP1,I)*CLRT1
             TRANL(LCTOP1)=QIKEXP( RJUNK1 )
             TRANZ(I)=QIKEXP( RJUNK1 - TAUZ(LCTOP1-1,I) )
             TRANS(I)=QIKEXP( CLRT1*(TAUZSN(LCTOP1-1,I)-  &
                TAUZSN(LCTOP1,I)) - TAUZSN(LCTOP1-1,I) )
             RSURFC=CEMIS1(I)*C1V3/( EXP( C2V/TCTOP1 ) - 1.0 )
             RHOTHR(I)=CRHOT1(I)
             RHOSUN(I)=CRHOS1(I)
             RPLNCK(LCTOP1)=C1V3/( EXP( C2V/TEMPC1 ) - 1.0 )
!             RSURFC=C1V3/( EXP( C2V/TEMPC1 ) - 1.0 )
         END IF
        
!        Calculate top cloud1 radiance
         IF (CFRA1X > 0.0) THEN
           ICH = I
           IF (LBLAC1) THEN
             CALL CALRAD0( DOSUN, ICH, LCTOP1, RPLNCK, RSURFC, SECANG,  &
               TRANL, TRANZ, SUNFAC, HSUN, TRANS, RHOSUN,  &
               RHOTHR, LABOVE, COEFF, RADC1 )
           ELSE
             CALL CALRAD1( DOSUN, ICH, LBOT, RPLNCK, RSURFE, SECANG,  &
               TAU, TRANL, TRANZ, SUNFAC, HSUN, TRANS, RHOSUN,  &
               RHOTHR, LABOVE, COEFF, CFRCL1, MASEC1, MASUN1, COSDAZ,  &
               NEXTO1, NSCAO1, G_ASY1, LCTOP1, LCBOT1, RADC1 )
           END IF
         ELSE
            RADC1=0.0
         END IF
         
!        Total the clear & various cloudy radiances
!         RAD(I)=RAD0*FCLEAR + RADC1*CFRA1X + RADC2*CFRA2X +  &
!            RADC12*CFRA12
         RAD(I)=RAD0
         
!cc this block for testing
         IF (I == 1291) THEN
!         print *,'chan1291 : iPROF,rad0,radc1,radc2,radc12,FINAL=',
!     $      IPROF,RAD0,RADC1,RADC2,RADC12,RAD(I)
!         print *,'chan1291 : IPROF,rad0,FCLEAR,CFRA1X,CFRA2X,CFRA12=',
!     $      IPROF,RAD0,FCLEAR,CFRA1X,CFRA2X,CFRA12
!         PRINT *,'CLOUD1 emis,temp = ',CEMIS1(I),TCTOP1
!         PRINT *,'CLOUD2 emis,temp = ',CEMIS2(I),TCTOP2
         END IF
!cc
         
      ENDDO ! channels
      
      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       
     if (COFNTE) then
! -------------------------
!  calculate nonLTE
! -------------------------
!      comment: the nonLTE calculation does not consider cloud effects,
!      but clouds are generally below the altitude where nonLTE occurs.
! first version call 'calnte.o' for original 90-deg nonLTE
!       if (DOSUN) THEN
!          CALL CALNTE ( INDCHN, TEMP, SUNCOS, SCOS1, SECANG(1), &
!                NCHNTE, CLISTN, COEFN, CO2TOP, RAD )
!       endif
! second version call 'calxnte.o' for extended to 120.deg nonLTE
        CALL CALNTE ( INDCHN, TEMP, SECANG(1), NCHNTE, CLISTN, &
         COEFN, CO2TOP, SUNANG, ALT(1), RAD )
 
     endif

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
