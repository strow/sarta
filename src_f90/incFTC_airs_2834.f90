MODULE incFTC
! include file
! 
! 
!
! Control output to stdout for help debugging
LOGICAL, PARAMETER :: DEBUG = .FALSE.
LOGICAL, PARAMETER :: CFCO2 = .TRUE.
LOGICAL, PARAMETER :: CFHNO3 = .TRUE.
LOGICAL, PARAMETER :: CFN2O = .TRUE.
LOGICAL, PARAMETER :: CFNH3 = .TRUE.
LOGICAL, PARAMETER :: CFSO2 = .TRUE.
LOGICAL, PARAMETER :: CFHDO = .FALSE.
LOGICAL, PARAMETER :: CFTHER = .TRUE.
LOGICAL, PARAMETER :: CFOPTR = .TRUE.
LOGICAL, PARAMETER :: COFNTE = .TRUE.

! ----------------
! I/O unit numbers
! ----------------
!      Note: these units are not explicitly openned by the sarta code,
!      they should be set to standard I/O units for your compiler
! IOINFO  ! unit number for non-error info messages (6)
! IOERR   ! unit number for error messages (2 or 6)
integer, PARAMETER :: IOINFO = 6 
integer, PARAMETER :: IOERR = 0 


! include variables and constants.
real, PARAMETER :: pi = 3.1415926535
real, PARAMETER :: DISTES=1.496E+11  ! distance Earth to Sun
real, PARAMETER ::  C1 = 1.191042722E-8     ! radiation constant (JPL value?)
!  C2    Current values (CODATA98 from NIST); agrees w/JPL Dec2000
real, PARAMETER :: C2 = 1.4387752
!RADSUN ! radius of the sun (6.956E+8 m)
real, PARAMETER :: RADSUN = 6.956E+8

! CO2STD  standard CO2 PPMV mixing ratio (400)
real, PARAMETER :: CO2STD = 400.0 
! DEG2RAD = pi/180 = degrees to radians conversion factor (was CONV)
real, PARAMETER :: DEG2RAD = 1.7453292E-02

! Generic Version specifiers
! version template    '#.## YYYY-MM-DD <--------comment------->'
character(len=40), PARAMETER :: VSARTA = '2.20 prod_2022' ! SARTA source code version
character(len=40), PARAMETER :: VSCOEF = 'AIRS L1C Jul-2022'  ! SARTA coefficient version
character(len=40), PARAMETER :: VTUNNG = 'none'         ! optical depth tuning version


! expected nominal satellite altitude (km)
real(4), PARAMETER :: XSALT = 705.0


! Bandpass, Layers, Gases
integer, PARAMETER :: MXCHAN = 2834          ! AIRS L1C 
integer, PARAMETER :: MXGAS = 44
integer, PARAMETER :: MXEMIS = 100
integer, PARAMETER :: MAXPRO = 25           ! max # of user specified profiles
integer, PARAMETER :: MAXLAY = 100          ! max # atm. layers:
integer, PARAMETER :: NFCOEF = 6            ! ! # of downwelling thermal "F" factor coefs

! -----------------------------------------
! Allowed input GUC (Gas Units Code number)
! 1 =  molecules/cm^2, 2 = kilomoles/cm^2
! -----------------------------------------
integer, PARAMETER :: GUCIN = 1

!***********************************************************************
!      Variables for the coefficient sets
!***********************************************************************
!
!      --------------
!      For set1 = FWO
!      -------------
!      Used in part by modules: 12, 11, 10, 9, 8, 7, 6, 5, 3, 4b, 4a
! MXCHN1 ! max # of channels for set1 = FWO (567)
! N1CON ! # of water con predictors/coefs for set1 (5)
! N1FIX ! # of "fixed" predictors/coefs for set1 (8)
! N1H2O ! # of water predictors/coefs for set1 (13)
! N1O3 ! # of ozone predictors/coefs for set1 (5)
! N1COEF ! total # of coefs for set1
integer, PARAMETER :: MXCHN1 = 1461
integer, PARAMETER :: N1CON = 7
integer, PARAMETER :: N1FIX = 8
integer, PARAMETER :: N1H2O = 11
integer, PARAMETER :: N1O3 = 5
integer, PARAMETER :: N1COEF = N1CON + N1FIX + N1H2O + N1O3
!
!      --------------
!      For set2 = FOW
!      --------------
!      Used in part by modules: 6, 5
! MXCHN2 ! max # of channels for set2 = FOW  (150)
! N2CON ! # of water con predictors/coefs for set2 (5)
! N2FIX ! # of "fixed" predictors/coefs for set2 (8)
! N2O3 ! # of ozone predictors/coefs for set2 (10)
! N2H2O ! # of water predictors/coefs for set2 (11)
! N2COEF ! total # of coefs for set2
integer, PARAMETER :: MXCHN2 = 325
integer, PARAMETER :: N2CON = 7
integer, PARAMETER :: N2FIX = 8
integer, PARAMETER :: N2O3 = 10
integer, PARAMETER :: N2H2O = 11
integer, PARAMETER :: N2COEF = N2CON + N2FIX + N2O3 + N2H2O
!
!      --------------
!      For set3 = FMW
!      --------------
!      Used in part by modules: 4d, 4c, 3
! MXCHN3 ! max # of channels for set3 = FMW  (653)
! N3CON ! # of water con predictors/coefs for set3 (5)
! N3FIX ! # of "fixed" predictors/coefs for set3 (8)
! N3CH4 ! # of methane predictors/coefs for set3 (9)
! N3H2O ! # of water predictors/coefs for set3 (13)
! N3COEF ! total # of coefs for set3
integer, PARAMETER :: MXCHN3 = 396
integer, PARAMETER :: N3CON = 7
integer, PARAMETER :: N3FIX = 8
integer, PARAMETER :: N3CH4 = 9
integer, PARAMETER :: N3H2O = 11
integer, PARAMETER ::N3COEF = N3CON + N3FIX + N3CH4 + N3H2O
!
!      ---------------
!      For set4 = sun FCOW
!      ---------------
!      Used in part by modules: 2b
! MXCHN4 ! max # of channels for set4 = FCOW (73)
! N4CON ! # of water con predictors/coefs for set4 (5)
! N4FIX ! # of "fixed" predictors/coefs for set4 (11)
! N4CO ! # of CO predictors/coefs for set4 (11)
! N4O3 ! # of ozone predictors/coefs for set4 (3)
! N4H2O ! # of water predictors/coefs for set4 (13)
! N4COEF ! total # of coefs for set4
integer, PARAMETER :: MXCHN4 = 85
integer, PARAMETER :: N4CON = 7
integer, PARAMETER :: N4FIX = 11
integer, PARAMETER ::  N4CO = 11
integer, PARAMETER ::  N4O3 = 3
integer, PARAMETER :: N4H2O = 13
integer, PARAMETER :: N4COEF = N4CON + N4FIX + N4CO + N4O3 + N4H2O 
!
!      -----------------------
!      For set5 = sun BFSW
!      -----------------------
!      Used in part by modules: 2b, 1b
! MXCHN5 ! max # of channels for set5 = BFSW (120)
! N5CON ! # of water con predictors/coefs for set5 (5)
! N5FIX ! # of "fixed" predictors/coefs for set5 (11)
! N5H2O ! # of water predictors/coefs for set5 (3)
! N5O3 ! # of ozone predictors/coefs for set5 (1)
! N5COEF ! total # of coefs for set5
integer, PARAMETER :: MXCHN5 = 210
integer, PARAMETER :: N5CON = 7
integer, PARAMETER :: N5FIX = 11
integer, PARAMETER :: N5H2O = 3
integer, PARAMETER ::  N5O3 = 1
integer, PARAMETER :: N5COEF = N5CON + N5FIX + N5H2O + N5O3
!
!      -----------------------
!      For set6 = sun MFMW
!      -----------------------
!      Used in part by modules: 1b, 2a
! MXCHN6 ! max # of channels for set6 = MFMW (128)
! N6CON ! # of water con predictors/coefs for set6 (5)
! N6FIX ! # of "fixed" predictors/coefs for set6 (8)
! N6H2O ! # of water predictors/coefs for set6 (7)
! N6O3 ! # of ozone predictors/coefs for set6 (1)
! N6COEF ! total # of coefs for set6
integer, PARAMETER :: MXCHN6 = 217
integer, PARAMETER :: N6CON = 7 
integer, PARAMETER :: N6FIX = 8
integer, PARAMETER :: N6H2O = 7
integer, PARAMETER ::  N6O3 = 1
integer, PARAMETER :: N6COEF = N6CON + N6FIX + N6H2O + N6O3
!
!      -----------------------
!      For set7 = sun MFBW
!      -----------------------
!      Used in part by modules: 2a, 1a
! MXCHN7 ! max # of channels for set7 = MFBW (0)
! N7CON ! # of water con predictors/coefs for set7 (5)
! N7FIX ! # of "fixed" predictors/coefs for set7 (8)
! N7H2O ! # of water predictors/coefs for set7 (13)
! N7O3 ! # of ozone predictors/coefs for set7 (1)
! N7COEF ! total # of coefs for set7
integer, PARAMETER :: MXCHN7 = 140
integer, PARAMETER :: N7CON = 7
integer, PARAMETER :: N7FIX = 8
integer, PARAMETER :: N7H2O = 13
integer, PARAMETER ::  N7O3 = 1
integer, PARAMETER :: N7COEF = N7CON + N7FIX + N7H2O + N7O3
!
!      ---------------
!      For trace gases predictors
!      ---------------
! NTRACE ! number of trace gas perturbation predictors (7)
integer, PARAMETER :: NTRACE = 7
!
!      ----------------
!      For variable CO2
!      ----------------
!      Used in part by modules: 12, 11, 10, 9, 7, 6, 5, 2b, 1b, 2a
! MXCHNC ! max # of channels with CO2 pert coefs (689)
! NCO2   ! number of CO2 pert predictors/coefs (5)
integer, PARAMETER :: MXCHNC = 1632    ! placeholder
integer, PARAMETER :: NCO2 = 5
!
!      ----------------
!      For variable SO2
!      ----------------
! MXCHNS ! max # of channels with SO2 pert coefs (was 270)
! NSO2 ! number of SO2 coefficients
integer, PARAMETER :: MXCHNS = 602
integer, PARAMETER :: NSO2 = 4
!
!      -----------------
!      For variable HNO3
!      -----------------
! MXCHNH ! max # of channels with HNO3 pert coefs (435)
!  NHNO3 ! number of HNO3 coefficients
integer, PARAMETER :: MXCHNH = 383
integer, PARAMETER :: NHNO3 = 4
!
!      -----------------
!      For variable N2O
!      -----------------
! MXCHNN ! max # of channels with N2O pert coefs (300)
! NN2O ! number of N2O coefficients
integer, PARAMETER :: MXCHNN = 586
integer, PARAMETER :: NN2O = 7
!
!      -----------------
!      For variable NH3
!      -----------------
! MXCHNA ! max # of channels with NH3 pert coefs (950)
! NNH3 ! number of NH3 coefficients (4)
integer, PARAMETER :: MXCHNA = 1422
integer, PARAMETER :: NNH3 = 4
!
!      -----------------
!      For variable HDO
!      -----------------
! MXCHND ! max # of channels with HDO pert coefs (2075)
! NHDO ! number of HDO coefficients (4)
! integer, PARAMETER :: MXCHND = 1        ! placeholder when not using this set
! integer, PARAMETER :: NHDO = 1         ! placeholder when not using this set
integer, PARAMETER :: MXCHND = 1843
integer, PARAMETER :: NHDO = 11

!      ----------------------
!      For OPTRAN water coefs
!      ----------------------
!      Used in part by modules:
! MXCHNW ! max # of channelss with OPTRAN H2O coefs (593)
! MXOWLY ! number of OPTRAN water layers
! NOWAVG ! # of OPTRAN water average profile values (4)
! NH2O   ! number of OPTRAN H2O predictors/coefs (9)
integer, PARAMETER :: MXCHNW = 754
integer, PARAMETER :: MXOWLY = 300
integer, PARAMETER :: NOWAVG = 4
integer, PARAMETER :: NH2O = 9
!
!      -----------
!      For non-LTE
!      -----------
! MXCNTE ! max # of channels for non-LTE (70)
! NNCOEF ! # of coefs for non-LTE (7)
! NTEBOT ! bottom layer for CO2TOP calc
! CO2NTE ! ref CO2 mixing ratio for non-LTE coefs (ppmv)
integer, PARAMETER :: MXCNTE = 134        ! was 133 placeholder
integer, PARAMETER :: NNCOEF = 14         ! was 7
integer, PARAMETER :: NTEBOT = 10
integer, PARAMETER :: CO2NTE = 400.0
integer, parameter :: NCHNTE = 172 
!
! ***********************************************************************

! Coefficient file names
character(len=80), PARAMETER :: FNCOF1='./dbase/airs/Coef/set1.dat'
character(len=80), PARAMETER :: FNCOF2='./dbase/airs/Coef/set2.dat'
character(len=80), PARAMETER :: FNCOF3='./dbase/airs/Coef/set3.dat'
character(len=80), PARAMETER :: FNCOF4='./dbase/airs/Coef/set4.dat'
character(len=80), PARAMETER :: FNCOF5='./dbase/airs/Coef/set5.dat'
character(len=80), PARAMETER :: FNCOF6='./dbase/airs/Coef/set6.dat'
character(len=80), PARAMETER :: FNCOF7='./dbase/airs/Coef/set7.dat'
character(len=80), PARAMETER :: FNOPTR ='./dbase/airs/Coef/optran_fmw.dat'
character(len=80), PARAMETER :: FNSO2 ='./dbase/airs/Coef/so2.dat'
character(len=80), PARAMETER :: FNN2O ='./dbase/airs/Coef/n2o.dat'
character(len=80), PARAMETER :: FNHNO3='./dbase/airs/Coef/hno3.dat'
character(len=80), PARAMETER :: FNNH3 ='./dbase/airs/Coef/nh3.dat'
character(len=80), PARAMETER :: FNHDO ='./dbase/airs/Coef/hdo.dat'
character(len=80), PARAMETER :: FNCO2 ='./dbase/airs/Coef/co2_5term.dat'
character(len=80), PARAMETER :: FNCOFN='./dbase/airs/Coef/xnlte_nn.txt'
character(len=80), PARAMETER :: FNTHER ='./dbase/airs/Coef/therm.dat'
!
character(len=80), PARAMETER :: FNSUN ='./dbase/airs/Solar/solardata_2834.txt'   ! solar data
character(len=80), PARAMETER :: FNFX  ='./dbase/airs/Coef/fx.txt'
character(len=80), PARAMETER :: FNTMLT='./dbase/airs/Coef/tunmlt_ones.txt'  ! tuning multipliers
character(len=80), PARAMETER :: FNPREF= &
   '/home/chepplew/gitLib/sarta_f90/dbase/refprof_400ppm'   ! reference atm.

!      ----------------
!      I/O unit numbers
!     ----------------
!      Note: these units are not explicitly openned by the sarta code,
!      they should be set to standard I/O units for your compiler
!       INTEGER IOINFO  ! unit number for non-error info messages (6)
!       INTEGER IOERR   ! unit number for error messages (2 or 6)
!       PARAMETER( IOINFO = 6 )
!       PARAMETER( IOERR = 0 )
!
!    unit IOUN: used by routines RDCOEF and RDPROF.
       INTEGER,  PARAMETER :: IOUN = 11         ! I/O unit number


end MODULE incFTC
