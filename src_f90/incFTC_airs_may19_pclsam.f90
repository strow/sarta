!=======================================================================
 
! Code converted using TO_F90_LOOP by Alan Miller
! Date: 2023-04-04  Time: 16:44:55
 
!=======================================================================

!    University of Maryland Baltimore County [UMBC]

!    AIRS 2834 with LLS L1C channel centers

!    incFTC

!F77====================================================================


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
!    May 2019 version of the 100 layer AIRS fast model
!    code by L.L.Strow/S.Hannon/Hepplewhite.  This AIRS model
!    uses the same algorithm and source code (except for this
!    include file) as our other fast models.

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
! 14 Sep 2018 C Hepplewhite  Created AIRS L1C fast model
! 1  Mar 2019 C Hepplewhite  Added HDO (not used this version)
! 21 Jun 2019 C Hepplewhite  Updated reflected thermal.
! 27 Jul 2020 C Hepplewhite  New paths for database files.

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

LOGICAL :: DEBUG
PARAMETER(DEBUG = .FALSE.)

LOGICAL :: CFCO2
LOGICAL :: CFHNO3
LOGICAL :: CFN2O
LOGICAL :: CFNH3
LOGICAL :: CFSO2
LOGICAL :: CFHDO
  LOGICAL :: CFOPTR
  LOGICAL :: CFTHER
  LOGICAL :: COFNTE
  PARAMETER(CFCO2  = .TRUE.)
  PARAMETER(CFHNO3 = .TRUE.)
  PARAMETER(CFN2O  = .TRUE.)
  PARAMETER(CFNH3  = .TRUE.)
  PARAMETER(CFSO2  = .TRUE.)
  PARAMETER(CFHDO  = .FALSE.)
    PARAMETER(CFOPTR = .TRUE.)
    PARAMETER(COFNTE = .TRUE.)
    PARAMETER(CFTHER = .TRUE.)
    
    CHARACTER (LEN=40) :: VSARTA  ! SARTA source code version
    CHARACTER (LEN=40) :: VSCOEF  ! SARTA coefficient version
    CHARACTER (LEN=40) :: VTUNNG  ! optical depth tuning version
!      version template    '#.## YYYY-MM-DD <--------comment------->'
    PARAMETER( VSARTA = '2.01 2019-05-14' )
    PARAMETER( VSCOEF = 'AIRS L1C Dec-2018')
    PARAMETER( VTUNNG = 'none' )
    
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
    
    REAL :: HDOSTD ! standard HDO depletion abundance (3.1069E-5)
      PARAMETER( HDOSTD = 0.00031069 )
      
      REAL :: HDOFCT ! vary proportion of HDO in H2O from std depletion
!                  ! (-1: 100% enhancement, 0:std HDO or zero depletion,  1: 100% depleted))
        PARAMETER( HDOFCT = 0.00 )
        
        REAL :: XSALT ! expected nominal satellite altitude (km)
        PARAMETER( XSALT = 705.0 )
        
!      -----------------------------------
!      Channels and layers other variables
!      -----------------------------------
        INTEGER :: MAXLAY ! # of layers (100)
        INTEGER :: NSET ! # of coefficient data sets (7)
        INTEGER :: MXCHAN ! max total # of channels (2834)
        INTEGER :: NFCOEF ! # of downwelling thermal "F" factor coefs
        INTEGER :: MXEMIS ! max # of input emis/rho data points
        INTEGER :: MAXPRO ! max # of user specified profiles
        INTEGER :: MXGAS ! max # of gases in user profile
        INTEGER :: MXMIEA ! max # of mie particle sizes (cloud code only)
        PARAMETER(MAXLAY = 100)
        PARAMETER(  NSET = 7)
        PARAMETER(MXCHAN = 2834)
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
        INTEGER :: MXCHN1 ! max # of channels for set1 = FWO (1461)
        INTEGER :: N1CON ! # of water con predictors/coefs for set1 (5)
        INTEGER :: N1FIX ! # of "fixed" predictors/coefs for set1 (8)
        INTEGER :: N1H2O ! # of water predictors/coefs for set1 (13)
        INTEGER :: N1O3 ! # of ozone predictors/coefs for set1 (5)
        INTEGER :: N1COEF ! total # of coefs for set1
        PARAMETER(MXCHN1 = 1461)
        PARAMETER( N1CON = 7)
        PARAMETER( N1FIX = 8)
        PARAMETER( N1H2O = 11)
        PARAMETER(  N1O3 = 5)
        PARAMETER(N1COEF = N1CON + N1FIX + N1H2O + N1O3 )
        
        
!      --------------
!      For set2 = FOW
!      --------------
!      Used in part by modules: 6, 5
        INTEGER :: MXCHN2 ! max # of channels for set2 = FOW  (477)
        INTEGER :: N2CON ! # of water con predictors/coefs for set2 (5)
        INTEGER :: N2FIX ! # of "fixed" predictors/coefs for set2 (8)
        INTEGER :: N2O3 ! # of ozone predictors/coefs for set2 (10)
        INTEGER :: N2H2O ! # of water predictors/coefs for set2 (11)
        INTEGER :: N2COEF ! total # of coefs for set2
        PARAMETER(MXCHN2 = 325)
        PARAMETER( N2CON = 7)
        PARAMETER( N2FIX = 8)
        PARAMETER(  N2O3 = 10)
        PARAMETER( N2H2O = 11)
        PARAMETER(N2COEF = N2CON + N2FIX + N2O3 + N2H2O )
        
        
!      --------------
!      For set3 = FMW
!      --------------
!      Used in part by modules: 4d, 4c, 3
        INTEGER :: MXCHN3 ! max # of channels for set3 = FMW  (401)
        INTEGER :: N3CON ! # of water con predictors/coefs for set3 (5)
        INTEGER :: N3FIX ! # of "fixed" predictors/coefs for set3 (8)
        INTEGER :: N3CH4 ! # of methane predictors/coefs for set3 (9)
        INTEGER :: N3H2O ! # of water predictors/coefs for set3 (13)
        INTEGER :: N3COEF ! total # of coefs for set3
        PARAMETER(MXCHN3 = 396)
        PARAMETER( N3CON = 7)
        PARAMETER( N3FIX = 8)
        PARAMETER( N3CH4 = 9)
        PARAMETER( N3H2O = 11)
        PARAMETER(N3COEF = N3CON + N3FIX + N3CH4 + N3H2O )
        
        
!      ---------------
!      For set4 = sun FCOW
!      ---------------
!      Used in part by modules: 2b
        INTEGER :: MXCHN4 ! max # of channels for set4 = FCOW (67)
        INTEGER :: N4CON ! # of water con predictors/coefs for set4 (5)
        INTEGER :: N4FIX ! # of "fixed" predictors/coefs for set4 (11)
        INTEGER :: N4CO ! # of CO predictors/coefs for set4 (11)
        INTEGER :: N4O3 ! # of ozone predictors/coefs for set4 (3)
        INTEGER :: N4H2O ! # of water predictors/coefs for set4 (13)
        INTEGER :: N4COEF ! total # of coefs for set4
        PARAMETER(MXCHN4 = 85)
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
        INTEGER :: MXCHN5 ! max # of channels for set5 = BFSW (163)
        INTEGER :: N5CON ! # of water con predictors/coefs for set5 (5)
        INTEGER :: N5FIX ! # of "fixed" predictors/coefs for set5 (11)
        INTEGER :: N5H2O ! # of water predictors/coefs for set5 (3)
        INTEGER :: N5O3 ! # of ozone predictors/coefs for set5 (1)
        INTEGER :: N5COEF ! total # of coefs for set5
        PARAMETER(MXCHN5 = 210)
        PARAMETER( N5CON = 7)
        PARAMETER( N5FIX = 11)
        PARAMETER( N5H2O = 3)
        PARAMETER(  N5O3 = 1)
        PARAMETER(N5COEF = N5CON + N5FIX + N5H2O + N5O3 )
        
        
!      -----------------------
!      For set6 = sun MFMW
!      -----------------------
!      Used in part by modules: 1b, 2a
        INTEGER :: MXCHN6 ! max # of channels for set6 = MFMW (179)
        INTEGER :: N6CON ! # of water con predictors/coefs for set6 (5)
        INTEGER :: N6FIX ! # of "fixed" predictors/coefs for set6 (8)
        INTEGER :: N6H2O ! # of water predictors/coefs for set6 (7)
        INTEGER :: N6O3 ! # of ozone predictors/coefs for set6 (1)
        INTEGER :: N6COEF ! total # of coefs for set6
        PARAMETER(MXCHN6 = 217)
        PARAMETER( N6CON = 7 )
        PARAMETER( N6FIX = 8 )
        PARAMETER( N6H2O = 7 )
        PARAMETER(  N6O3 = 1 )
        PARAMETER(N6COEF = N6CON + N6FIX + N6H2O + N6O3 )
        
        
!      -----------------------
!      For set7 = sun MFBW
!      -----------------------
!      Used in part by modules: 2a, 1a
        INTEGER :: MXCHN7 ! max # of channels for set7 = MFBW (74)
        INTEGER :: N7CON ! # of water con predictors/coefs for set7 (5)
        INTEGER :: N7FIX ! # of "fixed" predictors/coefs for set7 (8)
        INTEGER :: N7H2O ! # of water predictors/coefs for set7 (13)
        INTEGER :: N7O3 ! # of ozone predictors/coefs for set7 (1)
        INTEGER :: N7COEF ! total # of coefs for set7
        PARAMETER(MXCHN7 = 140)
        PARAMETER( N7CON = 7)
        PARAMETER( N7FIX = 8)
        PARAMETER( N7H2O = 13)
        PARAMETER(  N7O3 = 1)
        PARAMETER(N7COEF = N7CON + N7FIX + N7H2O + N7O3 )
        
        
!      ---------------
!      For trace gases predictors
!      ---------------
        INTEGER :: NTRACE ! number of trace gas perturbation predictors (7)
        PARAMETER(NTRACE = 7)
        
        
!      ----------------
!      For variable CO2
!      ----------------
!      Used in part by modules: 12, 11, 10, 9, 7, 6, 5, 2b, 1b, 2a
        INTEGER :: MXCHNC ! max # of channels with CO2 pert coefs (1632)
        INTEGER :: NCO2   ! number of CO2 pert predictors/coefs (5)
        PARAMETER(MXCHNC = 1632)
        PARAMETER(  NCO2 = 5)
        
        
!      ----------------
!      For variable SO2
!      ----------------
        INTEGER :: MXCHNS ! max # of channels with SO2 pert coefs (602)
        INTEGER :: NSO2 ! number of SO2 coefficients
        PARAMETER(MXCHNS = 602)
        PARAMETER(  NSO2 = 4)
        
        
!      -----------------
!      For variable HNO3
!      -----------------
        INTEGER :: MXCHNH ! max # of channels with HNO3 pert coefs (383)
        INTEGER :: NHNO3 ! number of HNO3 coefficients
        PARAMETER(MXCHNH = 383)
        PARAMETER( NHNO3 = 4)
        
        
!      -----------------
!      For variable N2O
!      -----------------
        INTEGER :: MXCHNN ! max # of channels with N2O pert coefs (173)
        INTEGER :: NN2O ! number of N2O coefficients
        PARAMETER(MXCHNN = 586)
        PARAMETER(  NN2O = 7)
        
!      -----------------
!      For variable NH3
!      -----------------
        INTEGER :: MXCHNA ! max # of channels with NH3 pert coefs (2075)
        INTEGER :: NNH3 ! number of NH3 coefficients (4)
!       PARAMETER(MXCHNA = 1)        ! placeholder when not using this set
!       PARAMETER( NNH3 = 1)         ! placeholder when not using this set
        PARAMETER(MXCHNA = 1422)
        PARAMETER(  NNH3 = 4)
        
!      -----------------
!      For variable HDO (not used this version)
!      -----------------
        INTEGER :: MXCHND ! max # of channels with HDO pert coefs (2075)
          INTEGER :: NHDO ! number of HDO coefficients (4)
            PARAMETER(MXCHND = 1)        ! placeholder when not using this set
!       PARAMETER( NHDO = 1)         ! placeholder when not using this set
!       PARAMETER(MXCHND = 1843)
            PARAMETER(  NHDO = 8)
              
!      ----------------------
!      For OPTRAN water coefs
!      ----------------------
!      Used in part by modules:
              INTEGER :: MXCHNW ! max # of channelss with OPTRAN H2O coefs (481)
              INTEGER :: MXOWLY ! number of OPTRAN water layers (300)
              INTEGER :: NOWAVG ! # of OPTRAN water average profile values (4)
              INTEGER :: NH2O   ! number of OPTRAN H2O predictors/coefs (9)
              PARAMETER(MXCHNW = 754)
              PARAMETER(MXOWLY = 300)
              PARAMETER(NOWAVG = 4)
              PARAMETER(  NH2O = 9)
              
!      -----------
!      For non-LTE
!      -----------
              INTEGER :: MXCNTE ! max # of channels for non-LTE (70)
              INTEGER :: NNCOEF ! # of coefs for non-LTE (7)
              INTEGER :: NTEBOT ! bottom layer for CO2TOP calc
              REAL :: CO2NTE ! ref CO2 mixing ratio for non-LTE coefs (ppmv)
              PARAMETER(MXCNTE = 200)
              PARAMETER(NNCOEF = 7)
              PARAMETER(NTEBOT = 10)
              PARAMETER(CO2NTE = 400.0)
              
!      ---------
!      Filenames
!      ---------
              CHARACTER (LEN=80) :: FNCOF1 ! coef set1
              CHARACTER (LEN=80) :: FNCOF2 ! coef set2
              CHARACTER (LEN=80) :: FNCOF3 ! coef set3
              CHARACTER (LEN=80) :: FNCOF4 ! coef set4
              CHARACTER (LEN=80) :: FNCOF5 ! coef set5
              CHARACTER (LEN=80) :: FNCOF6 ! coef set6
              CHARACTER (LEN=80) :: FNCOF7 ! coef set7
              CHARACTER (LEN=80) :: FNCO2  ! coef CO2
              CHARACTER (LEN=80) :: FNSO2  ! coef SO2
              CHARACTER (LEN=80) :: FNHNO3 ! coef HNO3
              CHARACTER (LEN=80) :: FNNH3  ! coef NH3
              CHARACTER (LEN=80) :: FNHDO  ! coef HDO
                CHARACTER (LEN=80) :: FNN2O  ! coef N2O
                CHARACTER (LEN=80) :: FNOPTR ! coef optran
                CHARACTER (LEN=80) :: FNTHER ! coef therm
                CHARACTER (LEN=80) :: FNFX   ! coef fx
                CHARACTER (LEN=80) :: FNPREF ! ref prof
                CHARACTER (LEN=80) :: FNSUN  ! solar data
                CHARACTER (LEN=80) :: FNCOFN ! non-LTE
                
                
                PARAMETER(FNCOF1=  &
                    '/asl/rta/sarta_database/Data_AIRS_may19/Coef/set1.dat')
                PARAMETER(FNCOF2=  &
                    '/asl/rta/sarta_database/Data_AIRS_may19/Coef/set2.dat')
                PARAMETER(FNCOF3=  &
                    '/asl/rta/sarta_database/Data_AIRS_may19/Coef/set3.dat')
                PARAMETER(FNCOF4=  &
                    '/asl/rta/sarta_database/Data_AIRS_may19/Coef/set4.dat')
                PARAMETER(FNCOF5=  &
                    '/asl/rta/sarta_database/Data_AIRS_may19/Coef/set5.dat')
                PARAMETER(FNCOF6=  &
                    '/asl/rta/sarta_database/Data_AIRS_may19/Coef/set6.dat')
                PARAMETER(FNCOF7=  &
                    '/asl/rta/sarta_database/Data_AIRS_may19/Coef/set7.dat')
                PARAMETER(FNOPTR=  &
                    '/asl/rta/sarta_database/Data_AIRS_may19/Coef/optran.dat')
                
                PARAMETER(FNCO2 =  &
                    '/asl/rta/sarta_database/Data_AIRS_may19/Coef/co2.dat')
                PARAMETER(FNSO2 =  &
                    '/asl/rta/sarta_database/Data_AIRS_may19/Coef/so2.dat')
                PARAMETER(FNHNO3 =  &
                    '/asl/rta/sarta_database/Data_AIRS_may19/Coef/hno3.dat')
                PARAMETER(FNN2O =  &
                    '/asl/rta/sarta_database/Data_AIRS_may19/Coef/n2o.dat')
                PARAMETER(FNNH3 =  &
                    '/asl/rta/sarta_database/Data_AIRS_may19/Coef/nh3.dat')
                PARAMETER(FNHDO =  &
                      '/asl/rta/sarta_database/Data_AIRS_may19/Coef/hdo.dat')
                  
                  PARAMETER(FNFX  =  &
                      '/asl/rta/sarta_database/Data_AIRS_may19/Coef/fx.txt')
                  PARAMETER(FNPREF=  &
                      '/asl/rta/sarta_database/Data_AIRS_may19/Coef/refprof_trace400')
                  PARAMETER(FNSUN =  &
                      '/asl/rta/sarta_database/Data_AIRS_may19/Solar/sol.txt')
                  
                  PARAMETER(FNTHER=  &
                      '/asl/rta/sarta_database/Data_AIRS_may19/Coef/therm.dat')
                  PARAMETER(FNCOFN=  &
                      '/asl/rta/sarta_database/Data_AIRS_may19/Coef/nte_7term.dat')
                  
                  
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Tuning filename
                  CHARACTER (LEN=80) :: FNTMLT ! tuning multiplier filename
                  
                  PARAMETER(FNTMLT=  &
                      '/asl/rta/sarta_database/Data_AIRS_may19/Coef/tunmlt.txt')
                  
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
!      Note: GUCIN must be 1 or 2.  All gases in the input RTP
!      must be of this type.
                  PARAMETER( GUCIN = 1 ) ! GUC number for:  molecules/cm^2
!       PARAMETER( GUCIN = 2 ) ! GUC number for:  kilomoles/cm^2
                  
                  
! rtpV201 compatibility
!$$$       CHARACTER*40 VCLOUD
!$$$       PARAMETER( VCLOUD = 'no clouds' )
                  
! number of mie types : dust, liquid cloud, ice cloud
                  INTEGER :: NMIETY
                  PARAMETER(NMIETY=3)
                  
!      End of include file
