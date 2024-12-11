!=======================================================================
 
! Code converted using TO_F90_LOOP by Alan Miller
! Date: 2023-04-04  Time: 16:44:55
 
!=======================================================================

!    University of Maryland Baltimore County [UMBC]

!    AIRS (Atmospheric Infra-Red Sounder)

!    incFTC (basic, optran, co2, so2, nh3, nte)

!F77====================================================================


!ROUTINE NAME:
!    incFTC (include file)


!ABSTRACT:
!    Include file consisting of parameter statements to size various
!    arrays in the USEFAST related routines source code.


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
!    Include file for the January 2004 100 layer AIRS fast
!    Stand Alone RTA (SARTA) code by L.L.Strow/S.Hannon.

!    Parameter statements for the FTC routines.


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    none


!ROUTINE HISTORY:
! Date        Programmer     Comments
! ----------- -------------- -------------------------------------------
! 02 Nov 2004 Scott Hannon   Created for CrIS 0.8 cm OPD all bands
! 09 May 2018 C Hepplewhite  add NH3 variables
!    May 2019 C Hepplewhite  add HDO variables
!    Sep 2019 C Hepplewhite  add logicals to control trace gas calcs

!END====================================================================

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

!      *************
!      SWITCHES
!      ************
LOGICAL :: DEBUG
PARAMETER(DEBUG = .FALSE.)

LOGICAL :: CFCO2
LOGICAL :: CFHNO3
LOGICAL :: CFN2O
LOGICAL :: CFNH3
LOGICAL :: CFSO2
LOGICAL :: CFHDO
  LOGICAL :: CFTHER
  PARAMETER(CFCO2  = .FALSE.)
  PARAMETER(CFHNO3 = .FALSE.)
  PARAMETER(CFN2O  = .FALSE.)
  PARAMETER(CFNH3  = .FALSE.)
  PARAMETER(CFSO2  = .FALSE.)
  PARAMETER(CFHDO  = .TRUE.)
    PARAMETER(CFTHER = .FALSE.)
    
    CHARACTER (LEN=40) :: VSARTA  ! SARTA source code version
    CHARACTER (LEN=40) :: VSCOEF  ! SARTA coefficient version
    CHARACTER (LEN=60) :: VTUNNG  ! optical depth tuning version
!       CHARACTER*40 VCLOUD  ! cloud version
!      version template    '#.## YY-MM-DD <--------comment--------->'
    PARAMETER( VSARTA = '1.05 04-10-06' )
    PARAMETER( VSCOEF = '04-11-02 CrIS 0.8 cm OPD all bands' )
    PARAMETER( VTUNNG = 'none' )
!       PARAMETER( VCLOUD = 'no clouds' )
    
!      *********
!      VARIABLES
!      *********
!      Note: these should not be changed by the user
    
!      ------------------------
!      Constants and other data
!      ------------------------
    REAL :: PI ! pi, circle circumference/diameter (3.1415926)
    REAL :: RADSUN ! radius of the sun (6.956E+8 m)
    REAL :: C1 ! radiation constant c1 (1.1911E-8  W/(m2.st.(cm-1)4)
    REAL :: C2 ! radiation constant c2 (1.4387863 K/cm-1)
    PARAMETER(    PI = 3.1415926)
    PARAMETER(RADSUN = 6.956E+8)
    
!cc    Previously used values; agrees w/JPL pre-Dec2000
!cc    PARAMETER(    C1 = 1.1910439E-8)  ! JPL value is 1E+3 bigger
!cc    PARAMETER(    C2 = 1.4387687)
    
!      Current values (CODATA98 from NIST); agrees w/JPL Dec2000
    PARAMETER(  C1 = 1.191042722E-8)  ! JPL value is 1E+3 bigger
    PARAMETER(  C2 = 1.4387752)
    
    REAL :: CO2STD ! standard CO2 PPMV mixing ratio (385)
    PARAMETER( CO2STD = 400.0 )
    
    
    REAL :: HDOFCT ! vary proportion of HDO in H2O from std depletion
!                  ! (-1: 100% enhancement, 0:std HDO or zero depletion,  1: 100% depleted))
      PARAMETER( HDOFCT = 0.60 )
      
      REAL :: XSALT ! expected nominal satellite altitude (km)
      PARAMETER( XSALT = 841.0 )
!      -----------------------------------
!      Channels and layers other variables
!      -----------------------------------
      INTEGER :: MAXLAY ! # of layers (100)
      INTEGER :: NSET ! # of coefficient data sets (7)
      INTEGER :: MXCHAN ! max total # of channels (2235)
      INTEGER :: NFCOEF ! # of downwelling thermal "F" factor coefs
      INTEGER :: MXEMIS ! max # of input emis/rho data points
      INTEGER :: MAXPRO ! max # of user specified profiles
      INTEGER :: MXGAS ! max # of gases in user profile
      INTEGER :: MXMIEA ! max # of mie particle sizes
      PARAMETER(MAXLAY = 100)
      PARAMETER(  NSET = 7)
      PARAMETER(MXCHAN = 2235)
      PARAMETER(NFCOEF = 6)
      PARAMETER(MXEMIS = 100)
      PARAMETER(MAXPRO = 25)
      PARAMETER( MXGAS = 44)
      PARAMETER(MXMIEA = 10)
      
!***********************************************************************
!      Variables for the coefficient sets
!***********************************************************************
      
!      --------------
!      For set1 = FWO
!      -------------
!      Used in part by modules: 12, 11, 10, 9, 8, 7, 6, 5, 3, 4b, 4a
      INTEGER :: MXCHN1 ! max # of channels for set1 = FWO (493)
      INTEGER :: N1CON ! # of water con predictors/coefs for set1 (7)
      INTEGER :: N1FIX ! # of "fixed" predictors/coefs for set1 (8)
      INTEGER :: N1H2O ! # of water predictors/coefs for set1 (11)
      INTEGER :: N1O3 ! # of ozone predictors/coefs for set1 (5)
      INTEGER :: N1COEF ! total # of coefs for set1
      PARAMETER(MXCHN1 = 493)
!       PARAMETER(MXCHN1 = 1)
      PARAMETER( N1CON = 7)
      PARAMETER( N1FIX = 8)
      PARAMETER( N1H2O = 11)
      PARAMETER(  N1O3 = 5)
      PARAMETER(N1COEF = N1CON + N1FIX + N1H2O + N1O3 )
      
      
!      --------------
!      For set2 = FOW
!      --------------
!      Used in part by modules: 6, 5
      INTEGER :: MXCHN2 ! max # of channels for set2 = FOW  (228)
      INTEGER :: N2CON ! # of water con predictors/coefs for set2 (7)
      INTEGER :: N2FIX ! # of "fixed" predictors/coefs for set2 (8)
      INTEGER :: N2O3 ! # of ozone predictors/coefs for set2 (10)
      INTEGER :: N2H2O ! # of water predictors/coefs for set2 (11)
      INTEGER :: N2COEF ! total # of coefs for set2
      PARAMETER(MXCHN2 = 228)
!       PARAMETER(MXCHN2 = 1)
      PARAMETER( N2CON = 7)
      PARAMETER( N2FIX = 8)
      PARAMETER(  N2O3 = 10)
      PARAMETER( N2H2O = 11)
      PARAMETER(N2COEF = N2CON + N2FIX + N2O3 + N2H2O )
      
      
!      --------------
!      For set3 = FMW
!      --------------
!      Used in part by modules: 4d, 4c, 3
      INTEGER :: MXCHN3 ! max # of channels for set3 = FMW  (392)
      INTEGER :: N3CON ! # of water con predictors/coefs for set3 (5)
      INTEGER :: N3FIX ! # of "fixed" predictors/coefs for set3 (8)
      INTEGER :: N3CH4 ! # of methane predictors/coefs for set3 (9)
      INTEGER :: N3H2O ! # of water predictors/coefs for set3 (13)
      INTEGER :: N3COEF ! total # of coefs for set3
      PARAMETER(MXCHN3 = 873)
      PARAMETER( N3CON = 7)
      PARAMETER( N3FIX = 8)
      PARAMETER( N3CH4 = 9)
      PARAMETER( N3H2O = 11)
      PARAMETER(N3COEF = N3CON + N3FIX + N3CH4 + N3H2O )
      
      
!      ---------------
!      For set4 = sun FCOW
!      ---------------
!      Used in part by modules: 2b
      INTEGER :: MXCHN4 ! max # of channels for set4 = FCOW (55)
      INTEGER :: N4CON ! # of water con predictors/coefs for set4 (5)
      INTEGER :: N4FIX ! # of "fixed" predictors/coefs for set4 (11)
      INTEGER :: N4CO ! # of CO predictors/coefs for set4 (11)
      INTEGER :: N4O3 ! # of ozone predictors/coefs for set4 (3)
      INTEGER :: N4H2O ! # of water predictors/coefs for set4 (13)
      INTEGER :: N4COEF ! total # of coefs for set4
      PARAMETER(MXCHN4 = 126)
!       PARAMETER(MXCHN4 = 1)
      PARAMETER( N4CON = 7)
      PARAMETER( N4FIX = 11)
      PARAMETER(  N4CO = 11)
      PARAMETER(  N4O3 = 3)
      PARAMETER( N4H2O = 13)
      PARAMETER(N4COEF = N4CON + N4FIX + N4CO + N4O3 + N4H2O )
      
      
!      -----------------------
!      For set5 = sun BFSW
!      -----------------------
!      Used in part by modules: 2b, 1b
      INTEGER :: MXCHN5 ! max # of channels for set5 = BFSW (202)
      INTEGER :: N5CON ! # of water con predictors/coefs for set5 (5)
      INTEGER :: N5FIX ! # of "fixed" predictors/coefs for set5 (11)
      INTEGER :: N5H2O ! # of water predictors/coefs for set5 (3)
      INTEGER :: N5O3 ! # of ozone predictors/coefs for set5 (1)
      INTEGER :: N5COEF ! total # of coefs for set5
      PARAMETER(MXCHN5 = 75)
!       PARAMETER(MXCHN5 = 1)
      PARAMETER( N5CON = 7)
      PARAMETER( N5FIX = 11)
      PARAMETER( N5H2O = 3)
      PARAMETER(  N5O3 = 1)
      PARAMETER(N5COEF = N5CON + N5FIX + N5H2O + N5O3 )
      
      
!      -----------------------
!      For set6 = sun MFMW
!      -----------------------
!      Used in part by modules: 1b, 2a
      INTEGER :: MXCHN6 ! max # of channels for set6 = MFMW (174)
      INTEGER :: N6CON ! # of water con predictors/coefs for set6 (5)
      INTEGER :: N6FIX ! # of "fixed" predictors/coefs for set6 (8)
      INTEGER :: N6H2O ! # of water predictors/coefs for set6 (7)
      INTEGER :: N6O3 ! # of ozone predictors/coefs for set6 (1)
      INTEGER :: N6COEF ! total # of coefs for set6
      PARAMETER(MXCHN6 = 415)
!       PARAMETER(MXCHN6 = 1)
      PARAMETER( N6CON = 7 )
      PARAMETER( N6FIX = 8 )
      PARAMETER( N6H2O = 7 )
      PARAMETER(  N6O3 = 1 )
      PARAMETER(N6COEF = N6CON + N6FIX + N6H2O + N6O3 )
      
      
!      -----------------------
!      For set7 = sun MFBW
!      -----------------------
!      Used in part by modules: 2a, 1a
      INTEGER :: MXCHN7 ! max # of channels for set7 = MFBW (83)
      INTEGER :: N7CON ! # of water con predictors/coefs for set7 (5)
      INTEGER :: N7FIX ! # of "fixed" predictors/coefs for set7 (8)
      INTEGER :: N7H2O ! # of water predictors/coefs for set7 (13)
      INTEGER :: N7O3 ! # of ozone predictors/coefs for set7 (1)
      INTEGER :: N7COEF ! total # of coefs for set7
      PARAMETER(MXCHN7 = 25)
!       PARAMETER(MXCHN7 = 1)
      PARAMETER( N7CON = 7)
      PARAMETER( N7FIX = 8)
      PARAMETER( N7H2O = 13)
      PARAMETER(  N7O3 = 1)
      PARAMETER(N7COEF = N7CON + N7FIX + N7H2O + N7O3 )
      
!      ---------------
!      For trace gases predictors
!      ---------------
      INTEGER :: NTRACE ! number of trace gas perturbation predictors (placeholder 1)
      PARAMETER(NTRACE = 7)
      
!      ----------------
!      For variable CO2
!      ----------------
!      Used in part by modules: 12, 11, 10, 9, 7, 6, 5, 2b, 1b, 2a
      INTEGER :: MXCHNC ! max # of channels with CO2 pert coefs (868)
      INTEGER :: NCO2   ! number of CO2 pert predictors/coefs (4)
!       PARAMETER(MXCHNC = 1)        ! placeholder when not using this set
!       PARAMETER( NCO2 = 1)         ! placeholder when not using this set
!       PARAMETER(MXCHNC = 891)
      PARAMETER(MXCHNC = 893)
      PARAMETER(  NCO2 = 5)       ! either 4 or 5. (5 if fitted w/fit_co2_5term_fowp_sun)
      
      
!      ----------------------
!      For SO2
!      ----------------------
      INTEGER :: MXCHNS ! max # of channels with SO2 pert coefs (placeholder 1)
      INTEGER :: NSO2 ! number of SO2 coefficients (placeholder 1)
      PARAMETER(MXCHNS = 295)   ! was 295
      PARAMETER(  NSO2 = 4)     ! was 4
!       PARAMETER(MXCHNS = 1)     ! placeholder when not using this set
!       PARAMETER(  NSO2 = 1)     ! placeholder when not using this set
      
!      ----------------------
!      For HNO3
!      ---------------------
      INTEGER :: MXCHNH ! max # of channels with HNO3 pert coefs (placeholder 1)
      INTEGER :: NHNO3 ! number of HNO3 coefficients (placeholder 1)
!       PARAMETER(MXCHNH = 570)   ! was 570
!       PARAMETER( NHNO3 = 4)     ! was 4
      PARAMETER(MXCHNH = 1)     ! placeholder when not using this set
      PARAMETER( NHNO3 = 4)     ! placeholder when not using this set
      
!      ----------------------
!      For N2O
!      ----------------------
      INTEGER :: MXCHNN ! max # of channels with N2O pert coefs (placeholder 1)
      INTEGER :: NN2O ! number of N2O coefficients (placeholder 1)
!       PARAMETER(MXCHNN = 433)   ! was 289
      PARAMETER(  NN2O = 7)     ! was 7
      PARAMETER(MXCHNN = 1)     ! placeholder when not using this set
      
      
!      ----------------
!      For variable NH3
!      ----------------
      INTEGER :: MXCHNA ! max # of channels with NH3 pert coefs (391)
      INTEGER :: NNH3 ! number of NH3 coefficients
      PARAMETER(MXCHNA = 920)
      PARAMETER(  NNH3 = 4)
!       PARAMETER(MXCHNA = 1)      ! Placeholder when not using this set
!       PARAMETER(  NNH3 = 1)      ! Placeholder when not using this set
      
!      -----------------
!      For variable HDO (not used this version)
!      -----------------
      INTEGER :: MXCHND ! max # of channels with HDO pert coefs (2075)
        INTEGER :: NHDO ! number of HDO coefficients (4)
!       PARAMETER(MXCHND = 1)        ! placeholder when not using this set
!       PARAMETER( NHDO = 1)         ! placeholder when not using this set
          PARAMETER(MXCHND = 394)
          PARAMETER(  NHDO = 11)
            
!      ----------------------
!      For non-LTE - placeholder while no coef file
!      ----------------------
            INTEGER :: MXCNTE ! max # of channels for non-LTE (placeholder 1)
            INTEGER :: NNCOEF ! # of coefs for non-LTE (placeholder 1)
            INTEGER :: NTEBOT ! bottom layer for CO2TOP calc
            REAL :: CO2NTE ! ref CO2 mixing ratio for non-LTE coefs (ppmv)
            PARAMETER(MXCNTE = 264)
            PARAMETER(NNCOEF = 7)
!       PARAMETER(MXCNTE = 1)   ! placeholder when not using this set
!       PARAMETER(NNCOEF = 1)   ! placeholder when not using this set
            PARAMETER(NTEBOT = 10)
            PARAMETER(CO2NTE = 400.0)
            
!      ----------------------
!      For OPTRAN water coefs
!      ----------------------
!      Used in part by modules:
            INTEGER :: MXCHNW ! max # of channelss with OPTRAN H2O coefs (637)
            INTEGER :: MXOWLY ! number of OPTRAN water layers
            INTEGER :: NOWAVG ! # of OPTRAN water average profile values (4)
            INTEGER :: NH2O   ! number of OPTRAN H2O predictors/coefs (9)
!       PARAMETER(MXCHNW = 2235)   ! CrIS HR g4
            PARAMETER(MXCHNW = 873)     ! CrIS HR g4 (FMW) mid-wave band only
!       PARAMETER(MXCHNW = 865)
!       PARAMETER(MXCHNW = 1602)
!       PARAMETER(MXCHNW = 12)
!       PARAMETER(MXCHNW = 1)   ! placeholder when this set is not used
            PARAMETER(MXOWLY = 300)
            PARAMETER(NOWAVG = 4)
            PARAMETER(  NH2O = 9)           ! was 9
            
            
!      ---------
!      Filenames
!      ---------
            CHARACTER (LEN=90) :: FNCOF1 ! coef set1
            CHARACTER (LEN=90) :: FNCOF2 ! coef set2
            CHARACTER (LEN=90) :: FNCOF3 ! coef set3
            CHARACTER (LEN=90) :: FNCOF4 ! coef set4
            CHARACTER (LEN=90) :: FNCOF5 ! coef set5
            CHARACTER (LEN=90) :: FNCOF6 ! coef set6
            CHARACTER (LEN=90) :: FNCOF7 ! coef set7
            CHARACTER (LEN=90) :: FNCO2  ! coef co2
            CHARACTER (LEN=90) :: FNOPTR ! coef optran
            CHARACTER (LEN=90) :: FNN2O  ! coef N2O
            CHARACTER (LEN=90) :: FNSO2  ! coef SO2
            CHARACTER (LEN=90) :: FNHNO3 ! coef HNO3
            CHARACTER (LEN=90) :: FNNH3  ! coef NH3
            CHARACTER (LEN=90) :: FNHDO  ! coef HDO
              CHARACTER (LEN=90) :: FNTHER ! coef therm
              CHARACTER (LEN=90) :: FNFX   ! coef fx
              CHARACTER (LEN=90) :: FNPREF ! ref prof
              CHARACTER (LEN=90) :: FNSUN  ! solar data
              CHARACTER (LEN=90) :: FNCOFN ! non-LTE
              
              PARAMETER(FNCOF1='/home/chepplew/data/sarta/prod_2018/cris_hr_mar18/Coef/set1.dat')
              PARAMETER(FNCOF2='/home/chepplew/data/sarta/prod_2018/cris_hr_mar18/Coef/set2.dat')
              PARAMETER(FNCOF3='/home/chepplew/data/sarta/prod_2018/cris_hr_mar18/Coef/set3.dat')
              PARAMETER(FNCOF4='/home/chepplew/data/sarta/prod_2018/cris_hr_mar18/Coef/set4.dat')
              PARAMETER(FNCOF5='/home/chepplew/data/sarta/prod_2018/cris_hr_mar18/Coef/set5.dat')
              PARAMETER(FNCOF6='/home/chepplew/data/sarta/prod_2018/cris_hr_mar18/Coef/set6.dat')
              PARAMETER(FNCOF7='/home/chepplew/data/sarta/prod_2018/cris_hr_mar18/Coef/set7.dat')
              PARAMETER(FNCO2='/home/chepplew/data/sarta/prod_2018/cris_hr_mar18/Coef/co2.dat')
              PARAMETER(FNOPTR='/home/chepplew/data/sarta/prod_2018/cris_hr_mar18/Coef/optran.dat')
              PARAMETER(FNN2O='/home/chepplew/data/sarta/prod_2018/cris_hr_mar18/Coef/n2o.dat')
              PARAMETER(FNSO2='/home/chepplew/data/sarta/prod_2018/cris_hr_mar18/Coef/so2.dat')
              PARAMETER(FNHNO3='/home/chepplew/data/sarta/prod_2018/cris_hr_mar18/Coef/hno3.dat')
              PARAMETER(FNNH3='/home/chepplew/data/sarta/prod_2018/cris_hr_mar18/Coef/nh3.dat')
              PARAMETER(FNHDO='/home/chepplew/data/sarta/prod_2019/cris_hr/dec2018/dbase/Coef/hdo.dat')
              PARAMETER(FNTHER='/home/chepplew/data/sarta/prod_2018/cris_hr_mar18/Coef/therm.dat')
              PARAMETER(FNCOFN='/home/chepplew/data/sarta/prod_2018/cris_hr_mar18/Coef/nte_7term.dat')
              PARAMETER(FNFX='/home/chepplew/data/sarta/prod_2018/cris_hr_mar18/Coef/fx.txt')
              PARAMETER(FNPREF='/home/chepplew/data/sarta/prod_2018/cris_hr_mar18/Coef/refprof_nh3')
              PARAMETER(FNSUN='/home/chepplew/data/sarta/prod_2018/cris_hr_mar18/Solar/sol.txt')
              
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Tuning filename
              CHARACTER (LEN=80) :: FNTMLT ! tuning multiplier filename
              
              PARAMETER(FNTMLT='/home/chepplew/data/sarta/prod_2018/cris_hr_mar18/Coef/tunmlt.txt')
              
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
              
!      ----------------
!      I/O unit numbers
!      ----------------
!      Note: these units are not explicitly openned by the sarta code,
!      they should be set to standard I/O units for your compiler
              INTEGER :: IOINFO  ! unit number for non-error info messages (6)
              INTEGER :: IOERR   ! unit number for error messages (2 or 6)
              PARAMETER( IOINFO = 6 )
              PARAMETER( IOERR = 0 )
              
              
!      -----------------
!      Allowed input GUC (Gas Units Code number)
!      -----------------
              INTEGER :: GUCIN  ! The one & only allowed input GUC number
              PARAMETER( GUCIN = 1 ) ! GUC number for:  molecules/cm^2
!       PARAMETER( GUCIN = 2 ) ! GUC number for:  kilomoles/cm^2
!      Note: GUCIN must be 1 or 2.  All gases in the input RTP
!      must be of this type.
              
!      End of include file
