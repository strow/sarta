!=======================================================================
!
!    University of Maryland Baltimore County [UMBC]
!
!    IASI
!
!    incFTC
!
!F90====================================================================

MODULE incFTC
! include file
! 
! 
!
! Control output to stdout for help debugging
LOGICAL, PARAMETER :: DEBUG = .FALSE.
! Control switches to turn on/off minor gas calcs.
LOGICAL, PARAMETER :: CFCO2 = .TRUE.
LOGICAL, PARAMETER :: CFHNO3 = .TRUE.
LOGICAL, PARAMETER :: CFN2O = .TRUE.
LOGICAL, PARAMETER :: CFNH3 = .TRUE.
LOGICAL, PARAMETER :: CFSO2 = .TRUE.
LOGICAL, PARAMETER :: CFHDO = .FALSE.
LOGICAL, PARAMETER :: CFCH4 = .FALSE.
LOGICAL, PARAMETER :: CFTHER = .TRUE.
LOGICAL, PARAMETER :: CFOPTR = .TRUE.
LOGICAL, PARAMETER :: COFNTE = .FALSE.

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

! HDO depletion (std. abundance 3.1069E-5)
real, PARAMETER :: HDOSTD = 0.00031069
! HDO depletion factor: vary proportion of HDO in H2O from std depletion
!                  ! (-1: 100% enhancement, 0:std HDO or zero depletion,
!                  1: 100% depleted [per.mil]))
real, PARAMETER :: HDOFCT = 0.00

! DEG2RAD = pi/180 = degrees to radians conversion factor (was CONV)
real, PARAMETER :: DEG2RAD = 1.7453292E-02

! Generic Version specifiers
! version template    '#.## YYYY-MM-DD <--------comment------->'
character(len=40), PARAMETER :: VSARTA = '2.20 prod_2022' ! SARTA source code version
character(len=40), PARAMETER :: VSCOEF = 'IASI jul-2022'  ! SARTA coefficient version
character(len=40), PARAMETER :: VTUNNG = 'none'         ! optical depth tuning version

! expected nominal satellite altitude (km)
real(4), PARAMETER :: XSALT = 825.0


! Bandpass, Layers, Gases
integer, PARAMETER :: NSET = 7              ! # base coefficient sets
integer, PARAMETER :: MXCHAN = 8461          ! IASI MetOpA,B,C 
integer, PARAMETER :: MXGAS = 44
integer, PARAMETER :: MXEMIS = 100
integer, PARAMETER :: MAXPRO = 25           ! max # of user specified profiles
integer, PARAMETER :: MAXLAY = 100          ! max # atm. layers:
integer, PARAMETER :: NFCOEF = 6            ! ! # of downwelling thermal "F" factor coefs
! integer, PARAMETER :: MXMIEA = 10         ! max # mie scattering particle sizes (cloud) 

! -----------------------------------------
! Allowed input GUC (Gas Units Code number)
! 1 =  molecules/cm^2, 2 = kilomoles/cm^2
! -----------------------------------------
integer, PARAMETER :: GUCIN = 1

!***********************************************************************
!      Variables for the coefficient sets
!***********************************************************************

!      --------------
!      For set1 = FWO
!      -------------
!      Used in part by modules: 12, 11, 10, 9, 8, 7, 6, 5, 3, 4b, 4a
       INTEGER MXCHN1 ! max # of channels for set1 = FWO (3750)
       INTEGER  N1CON ! # of water con predictors/coefs for set1 (7)
       INTEGER  N1FIX ! # of "fixed" predictors/coefs for set1 (8)
       INTEGER  N1H2O ! # of water predictors/coefs for set1 (11)
       INTEGER   N1O3 ! # of ozone predictors/coefs for set1 (5)
       INTEGER N1COEF ! total # of coefs for set1
       PARAMETER(MXCHN1 = 3750)
       PARAMETER( N1CON = 7)
       PARAMETER( N1FIX = 8)
       PARAMETER( N1H2O = 11)
       PARAMETER(  N1O3 = 5)
       PARAMETER(N1COEF = N1CON + N1FIX + N1H2O + N1O3 )
!
!
!      --------------
!      For set2 = FOW
!      --------------
!      Used in part by modules: 6, 5
       INTEGER MXCHN2 ! max # of channels for set2 = FOW  (678)
       INTEGER  N2CON ! # of water con predictors/coefs for set2 (7)
       INTEGER  N2FIX ! # of "fixed" predictors/coefs for set2 (8)
       INTEGER   N2O3 ! # of ozone predictors/coefs for set2 (10)
       INTEGER  N2H2O ! # of water predictors/coefs for set2 (11)
       INTEGER N2COEF ! total # of coefs for set2
       PARAMETER(MXCHN2 = 678)
       PARAMETER( N2CON = 7)
       PARAMETER( N2FIX = 8)
       PARAMETER(  N2O3 = 10)
       PARAMETER( N2H2O = 11)
       PARAMETER(N2COEF = N2CON + N2FIX + N2O3 + N2H2O )
!
!
!      --------------
!      For set3 = FMW
!      --------------
!      Used in part by modules: 4d, 4c, 3
       INTEGER MXCHN3 ! max # of channels for set3 = FMW  (1514)
       INTEGER  N3CON ! # of water con predictors/coefs for set3 (7)
       INTEGER  N3FIX ! # of "fixed" predictors/coefs for set3 (8)
       INTEGER  N3CH4 ! # of methane predictors/coefs for set3 (9)
       INTEGER  N3H2O ! # of water predictors/coefs for set3 (11)
       INTEGER N3COEF ! total # of coefs for set3
       PARAMETER(MXCHN3 = 1514)
       PARAMETER( N3CON = 7)
       PARAMETER( N3FIX = 8)
       PARAMETER( N3CH4 = 9)
       PARAMETER( N3H2O = 11)
       PARAMETER(N3COEF = N3CON + N3FIX + N3CH4 + N3H2O )
!
!
!      ---------------
!      For set4 = sun FCOW
!      ---------------
!      Used in part by modules: 2b
       INTEGER MXCHN4 ! max # of channels for set4 = FCOW (299)
       INTEGER  N4CON ! # of water con predictors/coefs for set4 (7)
       INTEGER  N4FIX ! # of "fixed" predictors/coefs for set4 (11)
       INTEGER   N4CO ! # of CO predictors/coefs for set4 (11)
       INTEGER   N4O3 ! # of ozone predictors/coefs for set4 (3)
       INTEGER  N4H2O ! # of water predictors/coefs for set4 (13)
       INTEGER N4COEF ! total # of coefs for set4
       PARAMETER(MXCHN4 = 299)
       PARAMETER( N4CON = 7)
       PARAMETER( N4FIX = 11)
       PARAMETER(  N4CO = 11)
       PARAMETER(  N4O3 = 3)
       PARAMETER( N4H2O = 13)
       PARAMETER(N4COEF = N4CON + N4FIX + N4CO + N4O3 + N4H2O )
!
!
!      -----------------------
!      For set5 = sun BFSW
!      -----------------------
!      Used in part by modules: 2b, 1b
       INTEGER MXCHN5 ! max # of channels for set5 = BFSW (789)
       INTEGER  N5CON ! # of water con predictors/coefs for set5 (7)
       INTEGER  N5FIX ! # of "fixed" predictors/coefs for set5 (11)
       INTEGER  N5H2O ! # of water predictors/coefs for set5 (3)
       INTEGER   N5O3 ! # of ozone predictors/coefs for set5 (1)
       INTEGER N5COEF ! total # of coefs for set5
       PARAMETER(MXCHN5 = 789)
       PARAMETER( N5CON = 7)
       PARAMETER( N5FIX = 11)
       PARAMETER( N5H2O = 3)
       PARAMETER(  N5O3 = 1)
       PARAMETER(N5COEF = N5CON + N5FIX + N5H2O + N5O3 )
!
!
!      -----------------------
!      For set6 = sun MFMW
!      -----------------------
!      Used in part by modules: 1b, 2a
       INTEGER MXCHN6 ! max # of channels for set6 = MFMW (957)
       INTEGER  N6CON ! # of water con predictors/coefs for set6 (7)
       INTEGER  N6FIX ! # of "fixed" predictors/coefs for set6 (8)
       INTEGER  N6H2O ! # of water predictors/coefs for set6 (7)
       INTEGER   N6O3 ! # of ozone predictors/coefs for set6 (1)
       INTEGER N6COEF ! total # of coefs for set6
       PARAMETER(MXCHN6 = 957)
       PARAMETER( N6CON = 7 )
       PARAMETER( N6FIX = 8 )
       PARAMETER( N6H2O = 7 )
       PARAMETER(  N6O3 = 1 )
       PARAMETER(N6COEF = N6CON + N6FIX + N6H2O + N6O3 )
!
!
!      -----------------------
!      For set7 = sun MFBW
!      -----------------------
!      Used in part by modules: 2a, 1a
       INTEGER MXCHN7 ! max # of channels for set7 = MFBW (474)
       INTEGER  N7CON ! # of water con predictors/coefs for set7 (7)
       INTEGER  N7FIX ! # of "fixed" predictors/coefs for set7 (8)
       INTEGER  N7H2O ! # of water predictors/coefs for set7 (13)
       INTEGER   N7O3 ! # of ozone predictors/coefs for set7 (1)
       INTEGER N7COEF ! total # of coefs for set7
       PARAMETER(MXCHN7 = 474)
       PARAMETER( N7CON = 7)
       PARAMETER( N7FIX = 8)
       PARAMETER( N7H2O = 13)
       PARAMETER(  N7O3 = 1)
       PARAMETER(N7COEF = N7CON + N7FIX + N7H2O + N7O3 )
!
!
!      ---------------
!      For trace gases predictors
!      ---------------
       INTEGER NTRACE ! number of trace gas perturbation predictors (7)
       PARAMETER(NTRACE = 7)
!
!
!      ----------------
!      For variable CO2
!      ----------------
!      Used in part by modules: 12, 11, 10, 9, 7, 6, 5, 2b, 1b, 2a
       INTEGER MXCHNC ! max # of channels with CO2 pert coefs (3827)
       INTEGER NCO2   ! number of CO2 pert predictors/coefs (5)
       PARAMETER(MXCHNC = 2863)
       PARAMETER(  NCO2 = 5)
!
!
!      ----------------
!      For variable SO2
!      ----------------
       INTEGER MXCHNS ! max # of channels with SO2 pert coefs (1419)
       INTEGER   NSO2 ! number of SO2 coefficients (4)
       PARAMETER(MXCHNS = 1419)
       PARAMETER(  NSO2 = 4)
!
!
!      -----------------
!      For variable HNO3
!      -----------------
       INTEGER MXCHNH ! max # of channels with HNO3 pert coefs (921)
       INTEGER  NHNO3 ! number of HNO3 coefficients (4)
       PARAMETER(MXCHNH = 921)
       PARAMETER( NHNO3 = 4)
!
!
!      -----------------
!      For variable N2O
!      -----------------
       INTEGER MXCHNN ! max # of channels with N2O pert coefs (2075)
       INTEGER   NN2O ! number of N2O coefficients (7)
       PARAMETER(MXCHNN = 2075)
       PARAMETER(  NN2O = 7)
!
!
!      -----------------
!      For variable NH3
!      -----------------
       INTEGER MXCHNA ! max # of channels with NH3 pert coefs (2075)
       INTEGER   NNH3 ! number of NH3 coefficients (4)
!       PARAMETER(MXCHNA = 1)        ! placeholder when not using this
!       set
!       PARAMETER( NNH3 = 1)         ! placeholder when not using this
!       set
       PARAMETER(MXCHNA = 1422)
       PARAMETER(  NNH3 = 4)
!
!      -----------------
!      For variable HDO
!      -----------------
       INTEGER MXCHND ! max # of channels with HDO pert coefs (2075)
       INTEGER   NHDO ! number of HDO coefficients (4)
!       PARAMETER(MXCHND = 1)        ! placeholder when not using this
!       set
!       PARAMETER( NHDO = 1)         ! placeholder when not using this
!       set
       PARAMETER(MXCHND = 1538)
       PARAMETER(  NHDO = 11)
!
!      -------------------
!      For variable SW CH4
!      -------------------
       INTEGER MXCHNM ! max # of channels with CH4 pert coefs (#)
       INTEGER   NCH4 ! number of CH4 coefficients (7)
!       PARAMETER(MXCHNM = 1)        ! placeholder when not using this
!       set
!       PARAMETER( NCH4 = 1)         ! placeholder when not using this
!       set
       PARAMETER(MXCHNM = 855)
       PARAMETER(  NCH4 = 7)
!
!      ----------------------
!      For OPTRAN water coefs
!      ----------------------
!      Used in part by modules:
       INTEGER MXCHNW ! max # of channelss with OPTRAN H2O coefs (2559)
       INTEGER MXOWLY ! number of OPTRAN water layers (300)
       INTEGER NOWAVG ! # of OPTRAN water average profile values (4)
       INTEGER NH2O   ! number of OPTRAN H2O predictors/coefs (9)
       PARAMETER(MXCHNW = 2559)
       PARAMETER(MXOWLY = 300)
       PARAMETER(NOWAVG = 4)
       PARAMETER(  NH2O = 9)
!
!      -----------
!      For non-LTE
!      -----------
! LXNTE  ! Logical. T: load 14 coefficients for 0-120.deg, F: load 7 for
! 0-90.deg
! MXCNTE ! max # of channels for non-LTE (70)
! NNCOEF ! # of coefs for non-LTE (7)
! XNCOEF ! # of coefs to read from the database file
! NTEBOT ! bottom layer for CO2TOP calc
! CO2NTE ! ref CO2 mixing ratio for non-LTE coefs (ppmv)
logical, PARAMETER :: LXNTE = .FALSE.     ! F: 0-90 or T: 0-120.deg solzen
integer, PARAMETER :: MXCNTE = 800        ! was 133 placeholder
integer, PARAMETER :: NNCOEF = 7          ! Default: 7 but context see: LXNTE
integer, PARAMETER :: XNCOEF = 14         ! Default: 2 x NNCOEF -> COEFN(XN,M)
integer, PARAMETER :: NTEBOT = 10
integer, PARAMETER :: CO2NTE = 400.0
!!integer, parameter :: NCHNTE = 800      ! definition moved
!!integer, parameter :: NNNNTE = 4          ! first dimension of neural net params

!      ---------
!      Filenames
!      ---------
  character(len=90), PARAMETER :: FNCOF1 = &
     '/home/chepplew/gitLib/sarta_f90/dbase/iasi/Coef/set1.dat'
  character(len=90), PARAMETER :: FNCOF2 = &
     '/home/chepplew/gitLib/sarta_f90/dbase/iasi/Coef/set2.dat'
  character(len=90), PARAMETER :: FNCOF3 = &
     '/home/chepplew/gitLib/sarta_f90/dbase/iasi/Coef/set3.dat'
  character(len=90), PARAMETER :: FNCOF4 = &
     '/home/chepplew/gitLib/sarta_f90/dbase/iasi/Coef/set4.dat'
  character(len=90), PARAMETER :: FNCOF5 = &
     '/home/chepplew/gitLib/sarta_f90/dbase/iasi/Coef/set5.dat'
  character(len=90), PARAMETER :: FNCOF6 = &
     '/home/chepplew/gitLib/sarta_f90/dbase/iasi/Coef/set6.dat'
  character(len=90), PARAMETER :: FNCOF7 = &
     '/home/chepplew/gitLib/sarta_f90/dbase/iasi/Coef/set7.dat'
!
  character(len=90), PARAMETER :: FNOPTR = &
     '/home/chepplew/gitLib/sarta_f90/dbase/iasi/Coef/optran.dat'
  character(len=90), PARAMETER :: FNCO2 = &
     '/home/chepplew/gitLib/sarta_f90/dbase/iasi/Coef/co2.dat'
  character(len=90), PARAMETER :: FNSO2 = &
     '/home/chepplew/gitLib/sarta_f90/dbase/iasi/Coef/so2.dat'
  character(len=90), PARAMETER :: FNHNO3 = &
     '/home/chepplew/gitLib/sarta_f90/dbase/iasi/Coef/hno3.dat'
  character(len=90), PARAMETER :: FNN2O = &
     '/home/chepplew/gitLib/sarta_f90/dbase/iasi/Coef/n2o.dat'
  character(len=90), PARAMETER :: FNNH3 = &
     '/home/chepplew/gitLib/sarta_f90/dbase/iasi/Coef/nh3.dat'
  character(len=90), PARAMETER :: FNHDO = &
     '/home/chepplew/gitLib/sarta_f90/dbase/iasi/Coef/hdo.dat'
  character(len=90), PARAMETER :: FNCH4 = &
     '/home/chepplew/gitLib/sarta_f90/dbase/iasi/Coef/ch4.dat'
  character(len=90), PARAMETER :: FNTHER = &
     '/home/chepplew/gitLib/sarta_f90/dbase/iasi/Coef/therm.dat'
  character(len=90), PARAMETER :: FNCOFN = &
     '/home/chepplew/gitLib/sarta_f90/dbase/iasi/Coef/xnlte_ann_model.txt'
  character(len=90), PARAMETER :: FNFX = &
     '/home/chepplew/gitLib/sarta_f90/dbase/iasi/Coef/fx.txt'
  character(len=90), PARAMETER :: FNPREF = &
     '/home/chepplew/gitLib/sarta_f90/dbase/refprof_400ppm'
  character(len=90), PARAMETER :: FNSUN = &
     '/home/chepplew/gitLib/sarta_f90/dbase/iasi/Solar/solardata.txt'

!
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Tuning filename
  character(len=90), PARAMETER :: FNTMLT = &
     '/home/chepplew/gitLib/sarta_f90/dbase/iasi/Coef/tunmlt_ones.txt'


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!      ----------------
!      I/O unit numbers
!      ----------------
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
