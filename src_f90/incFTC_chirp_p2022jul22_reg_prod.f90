!=======================================================================
!
!    University of Maryland Baltimore County [UMBC]
!
!    CHIRP SARTA
!
!    incFTC
!
!F90====================================================================


!ROUTINE NAME:
!    incFTC (include file)


!ABSTRACT:
!    Include file consisting of parameter statements to size various
!    arrays in the SARTA related routines source code.


!CALL PROTOCOL:
!    none (include file)


!INPUT PARAMETERS:
!    none


!OUTPUT PARAMETERS:
!    none


!INPUT/OUTPUT PARAMETERS:
!    none


!RETURN VALUES:
!    none


!PARENT(S):
!    CALOKW
!    CALOWP
!    CALPAR
!    CALRAD
!    CALT1
!    CALT2
!    CALT3
!    CALT4
!    CALT5
!    CALT6
!    CALT7
!    FAKETZ
!    RDCOEF
!    RDLIST
!    RDPROF
!    RDSUN
!    SUNPAR
!    SARTA


!ROUTINES CALLED:
!    none


!FILES ACCESSED:
!    none


!COMMON BLOCKS
!    none


!DESCRIPTION:
!    April 2009 version of the 100 layer CrIS fast model
!    code by L.L.Strow/S.Hannon.  This CrIS model
!    uses the same algorithm and source code (except for this
!    include file) as our AIRS fast model.
!
!    Parameter statements for the FTC routines.


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    none


!ROUTINE HISTORY:
! Date        Programmer     Comments
! ----------- -------------- -------------------------------------------
! 27 Apr 2009 Scott Hannon   Created for CrIS April 2009 fast model
! 12 May 2009 Scott Hannon   Add VTUNNG string; delete VCLOUD
! 14 Sep 2018 C Hepplewhite  Updated IASI fast model
! 1  Mar 2019 C Hepplewhite  Added HDO
! 1  Jul 2019 C Hepplewhite  Added minor gas logicals
!    Mar 2021 C Hepplewhite  Added nh3, and other minor gases.
!    Jul 2022 C Hepplewhite  prod_2022, build: jul2022 HITRAN: 2020.

!END====================================================================
!
!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------
! Note: having an "implicit none" in both the include file & the main
! source code will cause some compilers to complain.
!       IMPLICIT NONE


!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
!      none


!-----------------------------------------------------------------------
!      EXTERNAL FUNCTIONS
!-----------------------------------------------------------------------
!      none


!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
!      none


!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
!      none


!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none


!-----------------------------------------------------------------------
!      EXECUTABLE CODE
!-----------------------------------------------------------------------
!      none

!      -----------------------------------------------------------------
!      Assign SARTA version strings
!      -----------------------------------------------------------------
!      The version strings consists of 3 parts: version number, date,
!      and comment.  The version date should be updated to the
!      current date whenever any portion of the code is updated.  The
!      version number consists of two parts; a major version to the
!      left of the decimal point, and a minor version to the right.
!      The major number should be incremented only when major changes
!      have been made to the overall SARTA code.  The minor number
!      should be incremented only when minor but non-trivial changes
!      are made to the code.  Bug fixes should generally be handled
!      with the version date, but a fix for a serious bug may warrant
!      a change to the minor version number.
!      See the "Doc/last_update.txt" file for a description of the
!      changes associated with every change of VSARTA.
!

 MODULE incFTC

       LOGICAL DEBUG
       PARAMETER(DEBUG = .FALSE.)
!
       LOGICAL CFCO2
       LOGICAL CFHNO3
       LOGICAL CFN2O
       LOGICAL CFNH3
       LOGICAL CFSO2
       LOGICAL CFHDO
       LOGICAL CFTHER
       LOGICAL CFOPTR
       LOGICAL COFNTE
       PARAMETER(CFCO2  = .TRUE.)
       PARAMETER(CFHNO3 = .TRUE.)
       PARAMETER(CFN2O  = .TRUE.)
       PARAMETER(CFNH3  = .TRUE.)
       PARAMETER(CFSO2  = .TRUE.)
       PARAMETER(CFHDO  = .TRUE.)
       PARAMETER(CFOPTR = .TRUE.)
       PARAMETER(CFTHER = .TRUE.)
       PARAMETER(COFNTE = .TRUE.)
!
! ----------------
! I/O unit numbers
! ----------------
!      Note: these units are not explicitly openned by the sarta code,
!      they should be set to standard I/O units for your compiler
! IOINFO  ! unit number for non-error info messages (6)
! IOERR   ! unit number for error messages (2 or 6)
integer, PARAMETER :: IOINFO = 6 
integer, PARAMETER :: IOERR = 0 
!    unit IOUN: used by routines RDCOEF and RDPROF.
INTEGER,  PARAMETER :: IOUN = 11         ! I/O unit number

! include variables and constants.
real, PARAMETER :: pi = 3.1415926535
real, PARAMETER :: DISTES=1.496E+11  ! distance Earth to Sun
real, PARAMETER ::  C1 = 1.191042722E-8     ! radiation constant (JPL value?)
!  C2    Current values (CODATA98 from NIST); agrees w/JPL Dec2000
real, PARAMETER :: C2 = 1.4387752
!RADSUN ! radius of the sun (6.956E+8 m)
real, PARAMETER :: RADSUN = 6.956E+8
! DEG2RAD = pi/180 = degrees to radians conversion factor (was CONV)
real, PARAMETER :: DEG2RAD = 1.7453292E-02

! CO2STD  standard CO2 PPMV mixing ratio (400)
real, PARAMETER :: CO2STD = 400.0 

! HDO depletion (std. abundance 3.1069E-5)
real, PARAMETER :: HDOSTD = 0.00031069
! HDO depletion factor: vary proportion of HDO in H2O from std depletion
!                  ! (-1: 100% enhancement, 0:std HDO or zero depletion,
!                  1: 100% depleted [per.mil]))
real, PARAMETER :: HDOFCT = 0.00

! XSALT ! expected nominal satellite altitude (km)
real, PARAMETER :: XSALT = 705.0 

! Generic Version specifiers
! version template    '#.## YYYY-MM-DD <--------comment------->'
! SARTA source code version
character(len=40), PARAMETER :: VSARTA = '2.20 prod_2022' 
! SARTA coefficient version
character(len=40), PARAMETER :: VSCOEF = 'CrIS FSR 0.8/0.6/0.4cm Hamming jul-2022'  
! optical depth tuning version
character(len=40), PARAMETER :: VTUNNG = 'none'         

!
!      -----------------------------------
!      Channels and layers other variables
!      -----------------------------------
! MAXLAY ! # of layers (100)
! NSET ! # of coefficient data sets (7)
! MXCHAN ! max total # of channels (1305)
! NFCOEF ! # of downwelling thermal "F" factor coefs 
! MXEMIS ! max # of input emis/rho data points
! MAXPRO ! max # of user specified profiles
! MXGAS ! max # of gases in user profile
! MXMIEA ! max # of mie particle sizes (cloud code only)
integer,  PARAMETER :: MAXLAY = 100
integer,  PARAMETER ::  NSET = 7
integer,  PARAMETER :: MXCHAN = 1702
integer,  PARAMETER :: NFCOEF = 6
integer,  PARAMETER :: MXEMIS = 100
integer,  PARAMETER :: MAXPRO = 25
integer,  PARAMETER :: MXGAS = 44
integer,  PARAMETER :: MXMIEA = 10
!
!      -----------------
!      Allowed input GUC (Gas Units Code number)
!      -----------------
!  GUCIN  ! The one & only allowed input GUC number
!      Note: GUCIN must be 1 or 2.  All gases in the input RTP
!      must be of this type.
integer,  PARAMETER :: GUCIN = 1  ! GUC number for:  molecules/cm^2
! integer, PARAMETER :: GUCIN = 2  ! GUC number for:  kilomoles/cm^2
!
!***********************************************************************
!      Variables for the coefficient sets
!***********************************************************************
!
!      --------------
!      For set1 = FWO
!      -------------
!      Used in part by modules: 12, 11, 10, 9, 8, 7, 6, 5, 3, 4b, 4a
       INTEGER MXCHN1 ! max # of channels for set1 = FWO (458)
       INTEGER  N1CON ! # of water con predictors/coefs for set1 (5)
       INTEGER  N1FIX ! # of "fixed" predictors/coefs for set1 (8)
       INTEGER  N1H2O ! # of water predictors/coefs for set1 (13)
       INTEGER   N1O3 ! # of ozone predictors/coefs for set1 (5)
       INTEGER N1COEF ! total # of coefs for set1
       PARAMETER(MXCHN1 = 569)
       PARAMETER( N1CON = 7)
       PARAMETER( N1FIX = 8)
       PARAMETER( N1H2O = 11)
       PARAMETER(  N1O3 = 5)
       PARAMETER(N1COEF = N1CON + N1FIX + N1H2O + N1O3 )
!
!      --------------
!      For set2 = FOW
!      --------------
!      Used in part by modules: 6, 5
       INTEGER MXCHN2 ! max # of channels for set2 = FOW  (267)
       INTEGER  N2CON ! # of water con predictors/coefs for set2 (5)
       INTEGER  N2FIX ! # of "fixed" predictors/coefs for set2 (8)
       INTEGER   N2O3 ! # of ozone predictors/coefs for set2 (10)
       INTEGER  N2H2O ! # of water predictors/coefs for set2 (11)
       INTEGER N2COEF ! total # of coefs for set2
       PARAMETER(MXCHN2 = 152)
       PARAMETER( N2CON = 7)
       PARAMETER( N2FIX = 8)
       PARAMETER(  N2O3 = 10)
       PARAMETER( N2H2O = 11)
       PARAMETER(N2COEF = N2CON + N2FIX + N2O3 + N2H2O )
!
!      --------------
!      For set3 = FMW
!      --------------
!      Used in part by modules: 4d, 4c, 3
       INTEGER MXCHN3 ! max # of channels for set3 = FMW  (421)
       INTEGER  N3CON ! # of water con predictors/coefs for set3 (5)
       INTEGER  N3FIX ! # of "fixed" predictors/coefs for set3 (8)
       INTEGER  N3CH4 ! # of methane predictors/coefs for set3 (9)
       INTEGER  N3H2O ! # of water predictors/coefs for set3 (13)
       INTEGER N3COEF ! total # of coefs for set3
       PARAMETER(MXCHN3 = 656)
       PARAMETER( N3CON = 7)
       PARAMETER( N3FIX = 8)
       PARAMETER( N3CH4 = 9)
       PARAMETER( N3H2O = 11)
       PARAMETER(N3COEF = N3CON + N3FIX + N3CH4 + N3H2O )
!
!      ---------------
!      For set4 = sun FCOW
!      ---------------
!      Used in part by modules: 2b
       INTEGER MXCHN4 ! max # of channels for set4 = FCOW (39)
       INTEGER  N4CON ! # of water con predictors/coefs for set4 (5)
       INTEGER  N4FIX ! # of "fixed" predictors/coefs for set4 (11)
       INTEGER   N4CO ! # of CO predictors/coefs for set4 (11)
       INTEGER   N4O3 ! # of ozone predictors/coefs for set4 (3)
       INTEGER  N4H2O ! # of water predictors/coefs for set4 (13)
       INTEGER N4COEF ! total # of coefs for set4
       PARAMETER(MXCHN4 = 75)
       PARAMETER( N4CON = 7)
       PARAMETER( N4FIX = 11)
       PARAMETER(  N4CO = 11)
       PARAMETER(  N4O3 = 3)
       PARAMETER( N4H2O = 13)
       PARAMETER(N4COEF = N4CON + N4FIX + N4CO + N4O3 + N4H2O )
!
!      -----------------------
!      For set5 = sun BFSW
!      -----------------------
!      Used in part by modules: 2b, 1b
       INTEGER MXCHN5 ! max # of channels for set5 = BFSW (58)
       INTEGER  N5CON ! # of water con predictors/coefs for set5 (5)
       INTEGER  N5FIX ! # of "fixed" predictors/coefs for set5 (11)
       INTEGER  N5H2O ! # of water predictors/coefs for set5 (3)
       INTEGER   N5O3 ! # of ozone predictors/coefs for set5 (1)
       INTEGER N5COEF ! total # of coefs for set5
       PARAMETER(MXCHN5 = 120)
       PARAMETER( N5CON = 7)
       PARAMETER( N5FIX = 11)
       PARAMETER( N5H2O = 3)
       PARAMETER(  N5O3 = 1)
       PARAMETER(N5COEF = N5CON + N5FIX + N5H2O + N5O3 )
!
!      -----------------------
!      For set6 = sun MFMW
!      -----------------------
!      Used in part by modules: 1b, 2a
       INTEGER MXCHN6 ! max # of channels for set6 = MFMW (60)
       INTEGER  N6CON ! # of water con predictors/coefs for set6 (5)
       INTEGER  N6FIX ! # of "fixed" predictors/coefs for set6 (8)
       INTEGER  N6H2O ! # of water predictors/coefs for set6 (7)
       INTEGER   N6O3 ! # of ozone predictors/coefs for set6 (1)
       INTEGER N6COEF ! total # of coefs for set6
       PARAMETER(MXCHN6 = 130)
       PARAMETER( N6CON = 7 )
       PARAMETER( N6FIX = 8 )
       PARAMETER( N6H2O = 7 )
       PARAMETER(  N6O3 = 1 )
       PARAMETER(N6COEF = N6CON + N6FIX + N6H2O + N6O3 )
!
!      -----------------------
!      For set7 = sun MFBW
!      -----------------------
!      Used in part by modules: 2a, 1a
       INTEGER MXCHN7 ! max # of channels for set7 = MFBW (2)
       INTEGER  N7CON ! # of water con predictors/coefs for set7 (5)
       INTEGER  N7FIX ! # of "fixed" predictors/coefs for set7 (8)
       INTEGER  N7H2O ! # of water predictors/coefs for set7 (13)
       INTEGER   N7O3 ! # of ozone predictors/coefs for set7 (1)
       INTEGER N7COEF ! total # of coefs for set7
       PARAMETER(MXCHN7 = 0)
       PARAMETER( N7CON = 7)
       PARAMETER( N7FIX = 8)
       PARAMETER( N7H2O = 13)
       PARAMETER(  N7O3 = 1)
       PARAMETER(N7COEF = N7CON + N7FIX + N7H2O + N7O3 )
!
!      ---------------
!      For trace gases predictors
!      ---------------
       INTEGER NTRACE ! number of trace gas perturbation predictors (7)
       PARAMETER(NTRACE = 7)
!
!      ----------------
!      For variable CO2
!      ----------------
!      Used in part by modules: 12, 11, 10, 9, 7, 6, 5, 2b, 1b, 2a
       INTEGER MXCHNC ! max # of channels with CO2 pert coefs (was 567)
       INTEGER NCO2   ! number of CO2 pert predictors/coefs (5)
       PARAMETER(MXCHNC = 689)    ! placeholder
       PARAMETER(  NCO2 = 5)
!
!      ----------------
!      For variable SO2
!      ----------------
       INTEGER MXCHNS ! max # of channels with SO2 pert coefs (was 212)
       INTEGER   NSO2 ! number of SO2 coefficients
       PARAMETER(MXCHNS = 270)    ! placeholder
       PARAMETER(  NSO2 = 4)
!
!      -----------------
!      For variable HNO3
!      -----------------
       INTEGER MXCHNH ! max # of channels with HNO3 pert coefs (was 253)
       INTEGER  NHNO3 ! number of HNO3 coefficients
       PARAMETER(MXCHNH = 435)    ! placeholder
       PARAMETER( NHNO3 = 4)
!
!      -----------------
!      For variable N2O
!      -----------------
       INTEGER MXCHNN ! max # of channels with N2O pert coefs (was 181)
       INTEGER   NN2O ! number of N2O coefficients
       PARAMETER(MXCHNN = 300)
       PARAMETER(  NN2O = 7)
!
!      -----------------
!      For variable NH3
!      -----------------
       INTEGER MXCHNA ! max # of channels with NH3 pert coefs (2075)
       INTEGER   NNH3 ! number of NH3 coefficients (4)
!       PARAMETER(MXCHNA = 1)        ! placeholder when not using this set
!       PARAMETER( NNH3 = 1)         ! placeholder when not using this set
       PARAMETER(MXCHNA = 950)
       PARAMETER(  NNH3 = 4)
!
!      -----------------
!      For variable HDO
!      -----------------
       INTEGER MXCHND ! max # of channels with HDO pert coefs (252)
       INTEGER   NHDO ! number of HDO coefficients (11)
!       PARAMETER(MXCHND = 1)        ! placeholder when not using this set
!       PARAMETER( NHDO = 1)         ! placeholder when not using this set
       PARAMETER(MXCHND = 252)
       PARAMETER(  NHDO = 11)
!
!      ----------------------
!      For OPTRAN water coefs
!      ----------------------
!      Used in part by modules:
       INTEGER MXCHNW ! max # of channelss with OPTRAN H2O coefs (873)
       INTEGER MXOWLY ! number of OPTRAN water layers
       INTEGER NOWAVG ! # of OPTRAN water average profile values (4)
       INTEGER NH2O   ! number of OPTRAN H2O predictors/coefs (9)
       PARAMETER(MXCHNW = 593)
       PARAMETER(MXOWLY = 300)
       PARAMETER(NOWAVG = 4)
       PARAMETER(  NH2O = 9)
!
!      -----------
!      For non-LTE
!      -----------
! LXNTE  ! Logical. T: load 14 coefficients for 0-120.deg, F: load 7 for 0-90.deg
! MXCNTE ! max # of channels for non-LTE (264)
! NNCOEF ! # of coefs for non-LTE (7)
! XNCOEF ! # of coefs to read from the database file
! NTEBOT ! bottom layer for CO2TOP calc
! CO2NTE ! ref CO2 mixing ratio for non-LTE coefs (ppmv)
logical, PARAMETER :: LXNTE = .FALSE.     ! F: 0-90 or T: 0-120.deg solzen
integer, PARAMETER :: MXCNTE = 133        ! was 133 placeholder
integer, PARAMETER :: NNCOEF = 7          ! Default: 7 but context see: LXNTE
integer, PARAMETER :: XNCOEF = 14         ! Default: 2 x NNCOEF -> COEFN(XN,M)
integer, PARAMETER :: NTEBOT = 10
integer, PARAMETER :: CO2NTE = 400.0
!! integer, parameter :: NCHNTE = 277
integer, parameter :: NNNNTE = 4          ! first dimension of neural net params
!
! ----- ---------
!      Filenames
! ---------------
!
  character(len=80), PARAMETER :: FNCOF1 = &
    '/home/chepplew/data/sarta/prod_2022/chirp/jul2022/dbase/Coef/set1.dat'
  character(len=80), PARAMETER :: FNCOF2 = &
    '/home/chepplew/data/sarta/prod_2022/chirp/jul2022/dbase/Coef/set2.dat'
  character(len=80), PARAMETER :: FNCOF3 = &
    '/home/chepplew/data/sarta/prod_2022/chirp/jul2022/dbase/Coef/set3.dat'
  character(len=80), PARAMETER :: FNCOF4 = &
    '/home/chepplew/data/sarta/prod_2022/chirp/jul2022/dbase/Coef/set4.dat'
  character(len=80), PARAMETER :: FNCOF5 = &
    '/home/chepplew/data/sarta/prod_2022/chirp/jul2022/dbase/Coef/set5.dat'
  character(len=80), PARAMETER :: FNCOF6 = &
    '/home/chepplew/data/sarta/prod_2022/chirp/jul2022/dbase/Coef/set6.dat'
  character(len=80), PARAMETER :: FNCOF7 = &
    '/home/chepplew/data/sarta/prod_2022/chirp/jul2022/dbase/Coef/set7.dat'
!       
  character(len=80), PARAMETER :: FNOPTR = &
    '/home/chepplew/data/sarta/prod_2022/chirp/jul2022/dbase/Coef/optran.dat'
  character(len=80), PARAMETER :: FNCO2 = &
    '/home/chepplew/data/sarta/prod_2022/chirp/jul2022/dbase/Coef/co2.dat'
  character(len=80), PARAMETER :: FNSO2 = &
    '/home/chepplew/data/sarta/prod_2022/chirp/jul2022/dbase/Coef/so2.dat'
  character(len=80), PARAMETER :: FNHNO3 = &
    '/home/chepplew/data/sarta/prod_2022/chirp/jul2022/dbase/Coef/hno3.dat'
  character(len=80), PARAMETER :: FNN2O = &
    '/home/chepplew/data/sarta/prod_2022/chirp/jul2022/dbase/Coef/n2o.dat'
  character(len=80), PARAMETER :: FNNH3 = &
    '/home/chepplew/data/sarta/prod_2022/chirp/jul2022/dbase/Coef/nh3.dat'
  character(len=80), PARAMETER :: FNHDO = &
    '/home/chepplew/data/sarta/prod_2022/chirp/jul2022/dbase/Coef/hdo.dat'
  character(len=80), PARAMETER :: FNCOFN = &
    '/home/chepplew/data/sarta/prod_2022/chirp/jul2022/dbase/Coef/nte_7term.dat'
  character(len=80), PARAMETER :: FNTHER = &
    '/home/chepplew/data/sarta/prod_2022/chirp/jul2022/dbase/Coef/therm.dat'
!
  character(len=80), PARAMETER :: FNFX = &
    '/home/chepplew/data/sarta/prod_2022/chirp/jul2022/dbase/Coef/fx.txt'
  character(len=80), PARAMETER :: FNSUN = &
    '/home/chepplew/data/sarta/prod_2022/chirp/jul2022/dbase/Solar/solardata.txt'
  character(len=80), PARAMETER :: FNPREF = &
    '/home/chepplew/gitLib/sarta_f90/dbase/refprof_400ppm'
     
!
! -------------------------------------------
!  FNTMLT ! tuning multiplier filename
! -------------------------------------------
  character(len=80),PARAMETER :: FNTMLT = &
     '/home/chepplew/data/sarta/prod_2022/chirp/jul2022/dbase/Coef/tunmlt_ones.txt'
!
! rtpV201 compatibility
 character(len=80), PARAMETER :: VCLOUD = 'no clouds' 

!      End of include file
end MODULE incFTC
