C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore County [UMBC]
C
C    AIRS (Atmospheric Infra-Red Sounder)
C
C    incFTC
C
!F77====================================================================


!ROUTINE NAME:
C    incFTC (include file)


!ABSTRACT:
C    Include file consisting of parameter statements to size various
C    arrays in the USEFAST related routines source code.


!CALL PROTOCOL:
C    none (include file)


!INPUT PARAMETERS:
C    none


!OUTPUT PARAMETERS:
C    none


!INPUT/OUTPUT PARAMETERS:
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    CALOKW
C    CALOWP
C    CALPAR
C    CALRAD
C    CALT1
C    CALT2
C    CALT3
C    CALT4
C    CALT5
C    CALT6
C    CALT7
C    FAKETZ
C    RDCOEF
C    RDLIST
C    RDPROF
C    RDSUN
C    SUNPAR
C    SARTA


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    none


!COMMON BLOCKS
C    none


!DESCRIPTION:
C    Include file for the January 2004 100 layer AIRS fast
C    Stand Alone RTA (SARTA) code by L.L.Strow/S.Hannon.
C
C    Parameter statements for the FTC routines.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C Date        Programmer     Comments
C ----------- -------------- -------------------------------------------
C 02 Nov 2004 Scott Hannon   Created for CrIS 0.8 cm OPD all bands
C 05 Feb 2007 Scott Hannon   Add XSALT
C 16 Mar 2007 Scott Hannon   Add non-LTE params MXCHNN, NNCOEF, FNCOFN
C 02 May 2007 Scott Hannon   Updated for SARTA V1.07
C 14 May 2008 Scott Hannon   Updated for v1.08; add CO2NTE and NTEBOT
C                            and increase NNCOEF from 6 to 7
C 12 May 2009 Scott Hannon   Add VTUNNG string; delete VCLOUD
C 14 Sep 2018 C Hepplewhite  Updated IASI fast model
C 1  Mar 2019 C Hepplewhite  Added HDO
C 1  Dec 2019 C Hepplewhite  v2.01 compatibility with IASI and AIRS_L1C

!END====================================================================
C
C-----------------------------------------------------------------------
C      IMPLICIT NONE
C-----------------------------------------------------------------------
C Note: having an "implicit none" in both the include file & the main
C source code will cause some compilers to complain.
c       IMPLICIT NONE


C-----------------------------------------------------------------------
C      INCLUDE FILES
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      EXTERNAL FUNCTIONS
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      SAVE STATEMENTS
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      EXECUTABLE CODE
C-----------------------------------------------------------------------
C      none

C      -----------------------------------------------------------------
C      Assign SARTA version strings
C      -----------------------------------------------------------------
C      The version strings consists of 3 parts: version number, date,
C      and comment.  The version date should be updated to the
C      current date whenever any portion of the code is updated.  The
C      version number consists of two parts; a major version to the
C      left of the decimal point, and a minor version to the right.
C      The major number should be incremented only when major changes
C      have been made to the overall SARTA code.  The minor number
C      should be incremented only when minor but non-trivial changes
C      are made to the code.  Bug fixes should generally be handled
C      with the version date, but a fix for a serious bug may warrant
C      a change to the minor version number.
C      See the "Doc/last_update.txt" file for a description of the
C      changes associated with every change of VSARTA.
C
       LOGICAL DEBUG
       PARAMETER(DEBUG = .FALSE.)
C
       LOGICAL CFCO2
       LOGICAL CFHNO3
       LOGICAL CFN2O
       LOGICAL CFNH3
       LOGICAL CFSO2
       LOGICAL CFHDO
       LOGICAL CFTHER
       PARAMETER(CFCO2  = .FALSE.)
       PARAMETER(CFHNO3 = .FALSE.)
       PARAMETER(CFN2O  = .FALSE.)
       PARAMETER(CFNH3  = .FALSE.)
       PARAMETER(CFSO2  = .FALSE.)
       PARAMETER(CFHDO  = .FALSE.)
       PARAMETER(CFTHER = .FALSE.)
C
       CHARACTER*40 VSARTA  ! SARTA source code version
       CHARACTER*40 VSCOEF  ! SARTA coefficient version
       CHARACTER*60 VTUNNG  ! optical depth tuning version
C       CHARACTER*40 VCLOUD  ! cloud version
C      version template    '#.## YY-MM-DD <--------comment--------->'
       PARAMETER( VSARTA = '2.01 2018-10-01' )
       PARAMETER( VSCOEF = 'Dec-2018 CrIS 0.8 cm OPD all bands' )
       PARAMETER( VTUNNG = 'none' )
C       PARAMETER( VCLOUD = 'no clouds' )

C      *********
C      VARIABLES
C      *********
C      Note: these should not be changed by the user
C
C      ------------------------
C      Constants and other data
C      ------------------------
       REAL     PI ! pi, circle circumference/diameter (3.1415926)
       REAL RADSUN ! radius of the sun (6.956E+8 m)
       REAL     C1 ! radiation constant c1 (1.1911E-8  W/(m2.st.(cm-1)4)
       REAL     C2 ! radiation constant c2 (1.4387863 K/cm-1)
       PARAMETER(    PI = 3.1415926)
       PARAMETER(RADSUN = 6.956E+8)
C
Ccc    Previously used values; agrees w/JPL pre-Dec2000
Ccc    PARAMETER(    C1 = 1.1910439E-8)  ! JPL value is 1E+3 bigger
Ccc    PARAMETER(    C2 = 1.4387687)
C
C      Current values (CODATA98 from NIST); agrees w/JPL Dec2000
       PARAMETER(  C1 = 1.191042722E-8)  ! JPL value is 1E+3 bigger
       PARAMETER(  C2 = 1.4387752)
C
       REAL CO2STD ! standard CO2 PPMV mixing ratio (385)
       PARAMETER( CO2STD = 400.0 )
C
       REAL HDOSTD ! standard HDO depletion abundance (3.1069E-5)
       PARAMETER( HDOSTD = 0.00031069 )
C
       REAL HDOFCT ! vary proportion of HDO in H2O from std depletion
C                  ! (-1: 100% enhancement, 0:std HDO or zero depletion,
C                  1: 100% depleted))
       PARAMETER( HDOFCT = 0.00 )
C
       REAL  XSALT ! expected nominal satellite altitude (km)
       PARAMETER( XSALT = 841.0 )
C      -----------------------------------
C      Channels and layers other variables
C      -----------------------------------
       INTEGER MAXLAY ! # of layers (100)
       INTEGER   NSET ! # of coefficient data sets (7)
       INTEGER MXCHAN ! max total # of channels (2235)
       INTEGER NFCOEF ! # of downwelling thermal "F" factor coefs 
       INTEGER MXEMIS ! max # of input emis/rho data points
       INTEGER MAXPRO ! max # of user specified profiles
       INTEGER  MXGAS ! max # of gases in user profile
       INTEGER MXMIEA ! max # of mie particle sizes
       PARAMETER(MAXLAY = 100)
       PARAMETER(  NSET = 7)
       PARAMETER(MXCHAN = 2235)
       PARAMETER(NFCOEF = 6)
       PARAMETER(MXEMIS = 100)
       PARAMETER(MAXPRO = 25)
       PARAMETER( MXGAS = 44)
       PARAMETER(MXMIEA = 10)
C
C***********************************************************************
C      Variables for the coefficient sets
C***********************************************************************
C
C      --------------
C      For set1 = FWO
C      -------------
C      Used in part by modules: 12, 11, 10, 9, 8, 7, 6, 5, 3, 4b, 4a
       INTEGER MXCHN1 ! max # of channels for set1 = FWO (493)
       INTEGER  N1CON ! # of water con predictors/coefs for set1 (7)
       INTEGER  N1FIX ! # of "fixed" predictors/coefs for set1 (8)
       INTEGER  N1H2O ! # of water predictors/coefs for set1 (11)
       INTEGER   N1O3 ! # of ozone predictors/coefs for set1 (5)
       INTEGER N1COEF ! total # of coefs for set1
       PARAMETER(MXCHN1 = 493)
c       PARAMETER(MXCHN1 = 1)
       PARAMETER( N1CON = 7)
       PARAMETER( N1FIX = 8)
       PARAMETER( N1H2O = 11)
       PARAMETER(  N1O3 = 5)
       PARAMETER(N1COEF = N1CON + N1FIX + N1H2O + N1O3 )
C
C
C      --------------
C      For set2 = FOW
C      --------------
C      Used in part by modules: 6, 5
       INTEGER MXCHN2 ! max # of channels for set2 = FOW  (228)
       INTEGER  N2CON ! # of water con predictors/coefs for set2 (7)
       INTEGER  N2FIX ! # of "fixed" predictors/coefs for set2 (8)
       INTEGER   N2O3 ! # of ozone predictors/coefs for set2 (10)
       INTEGER  N2H2O ! # of water predictors/coefs for set2 (11)
       INTEGER N2COEF ! total # of coefs for set2
       PARAMETER(MXCHN2 = 228)
c       PARAMETER(MXCHN2 = 1)
       PARAMETER( N2CON = 7)
       PARAMETER( N2FIX = 8)
       PARAMETER(  N2O3 = 10)
       PARAMETER( N2H2O = 11)
       PARAMETER(N2COEF = N2CON + N2FIX + N2O3 + N2H2O )
C
C
C      --------------
C      For set3 = FMW
C      --------------
C      Used in part by modules: 4d, 4c, 3
       INTEGER MXCHN3 ! max # of channels for set3 = FMW  (392)
       INTEGER  N3CON ! # of water con predictors/coefs for set3 (5)
       INTEGER  N3FIX ! # of "fixed" predictors/coefs for set3 (8)
       INTEGER  N3CH4 ! # of methane predictors/coefs for set3 (9)
       INTEGER  N3H2O ! # of water predictors/coefs for set3 (13)
       INTEGER N3COEF ! total # of coefs for set3
       PARAMETER(MXCHN3 = 873)
       PARAMETER( N3CON = 7)
       PARAMETER( N3FIX = 8)
       PARAMETER( N3CH4 = 9)
       PARAMETER( N3H2O = 11)
       PARAMETER(N3COEF = N3CON + N3FIX + N3CH4 + N3H2O )
C
C
C      ---------------
C      For set4 = sun FCOW
C      ---------------
C      Used in part by modules: 2b
       INTEGER MXCHN4 ! max # of channels for set4 = FCOW (55)
       INTEGER  N4CON ! # of water con predictors/coefs for set4 (5)
       INTEGER  N4FIX ! # of "fixed" predictors/coefs for set4 (11)
       INTEGER   N4CO ! # of CO predictors/coefs for set4 (11)
       INTEGER   N4O3 ! # of ozone predictors/coefs for set4 (3)
       INTEGER  N4H2O ! # of water predictors/coefs for set4 (13)
       INTEGER N4COEF ! total # of coefs for set4
       PARAMETER(MXCHN4 = 126)
c       PARAMETER(MXCHN4 = 1)
       PARAMETER( N4CON = 7)
       PARAMETER( N4FIX = 11)
       PARAMETER(  N4CO = 11)
       PARAMETER(  N4O3 = 3)
       PARAMETER( N4H2O = 13)
       PARAMETER(N4COEF = N4CON + N4FIX + N4CO + N4O3 + N4H2O )
C
C
C      -----------------------
C      For set5 = sun BFSW
C      -----------------------
C      Used in part by modules: 2b, 1b
       INTEGER MXCHN5 ! max # of channels for set5 = BFSW (202)
       INTEGER  N5CON ! # of water con predictors/coefs for set5 (5)
       INTEGER  N5FIX ! # of "fixed" predictors/coefs for set5 (11)
       INTEGER  N5H2O ! # of water predictors/coefs for set5 (3)
       INTEGER   N5O3 ! # of ozone predictors/coefs for set5 (1)
       INTEGER N5COEF ! total # of coefs for set5
       PARAMETER(MXCHN5 = 75)
c       PARAMETER(MXCHN5 = 1)
       PARAMETER( N5CON = 7)
       PARAMETER( N5FIX = 11)
       PARAMETER( N5H2O = 3)
       PARAMETER(  N5O3 = 1)
       PARAMETER(N5COEF = N5CON + N5FIX + N5H2O + N5O3 )
C
C
C      -----------------------
C      For set6 = sun MFMW
C      -----------------------
C      Used in part by modules: 1b, 2a
       INTEGER MXCHN6 ! max # of channels for set6 = MFMW (174)
       INTEGER  N6CON ! # of water con predictors/coefs for set6 (5)
       INTEGER  N6FIX ! # of "fixed" predictors/coefs for set6 (8)
       INTEGER  N6H2O ! # of water predictors/coefs for set6 (7)
       INTEGER   N6O3 ! # of ozone predictors/coefs for set6 (1)
       INTEGER N6COEF ! total # of coefs for set6
       PARAMETER(MXCHN6 = 415)
c       PARAMETER(MXCHN6 = 1)
       PARAMETER( N6CON = 7 )
       PARAMETER( N6FIX = 8 )
       PARAMETER( N6H2O = 7 )
       PARAMETER(  N6O3 = 1 )
       PARAMETER(N6COEF = N6CON + N6FIX + N6H2O + N6O3 )
C
C
C      -----------------------
C      For set7 = sun MFBW
C      -----------------------
C      Used in part by modules: 2a, 1a
       INTEGER MXCHN7 ! max # of channels for set7 = MFBW (83)
       INTEGER  N7CON ! # of water con predictors/coefs for set7 (5)
       INTEGER  N7FIX ! # of "fixed" predictors/coefs for set7 (8)
       INTEGER  N7H2O ! # of water predictors/coefs for set7 (13)
       INTEGER   N7O3 ! # of ozone predictors/coefs for set7 (1)
       INTEGER N7COEF ! total # of coefs for set7
       PARAMETER(MXCHN7 = 25)
c       PARAMETER(MXCHN7 = 1)
       PARAMETER( N7CON = 7)
       PARAMETER( N7FIX = 8)
       PARAMETER( N7H2O = 13)
       PARAMETER(  N7O3 = 1)
       PARAMETER(N7COEF = N7CON + N7FIX + N7H2O + N7O3 )
C
C      ---------------
C      For trace gases predictors
C      ---------------
       INTEGER NTRACE ! number of trace gas perturbation predictors (placeholder 1)
       PARAMETER(NTRACE = 7)
C
C      ----------------
C      For variable CO2
C      ----------------
C      Used in part by modules: 12, 11, 10, 9, 7, 6, 5, 2b, 1b, 2a
       INTEGER MXCHNC ! max # of channels with CO2 pert coefs (868)
       INTEGER NCO2   ! number of CO2 pert predictors/coefs (4)
C       PARAMETER(MXCHNC = 1)
C       PARAMETER( NCO2 = 1)
C       PARAMETER(MXCHNC = 891)
       PARAMETER(MXCHNC = 893)
       PARAMETER(  NCO2 = 5)       ! was 4-term. otherwise 5-term
C
C
C      ----------------------
C      For SO2
C      ----------------------
       INTEGER MXCHNS ! max # of channels with SO2 pert coefs (placeholder 1)
       INTEGER   NSO2 ! number of SO2 coefficients (placeholder 1)
       PARAMETER(MXCHNS = 295)   ! was 295
       PARAMETER(  NSO2 = 4)     ! was 4

C      ----------------------
C      For HNO3
C      ---------------------
       INTEGER MXCHNH ! max # of channels with HNO3 pert coefs (placeholder 1)
       INTEGER  NHNO3 ! number of HNO3 coefficients (placeholder 1)
       PARAMETER(MXCHNH = 570)   ! was 570
       PARAMETER( NHNO3 = 4)     ! was 4

C      ----------------------
C      For N2O
C      ----------------------
       INTEGER MXCHNN ! max # of channels with N2O pert coefs (placeholder 1)
       INTEGER   NN2O ! number of N2O coefficients (placeholder 1)
       PARAMETER(MXCHNN = 433)   ! was 289
       PARAMETER(  NN2O = 7)     ! was 7

C      -----------------
C      For variable NH3
C      -----------------
       INTEGER MXCHNA ! max # of channels with NH3 pert coefs (2075)
       INTEGER   NNH3 ! number of NH3 coefficients (4)
       PARAMETER(MXCHNA = 1)        ! placeholder when not using this set
C       PARAMETER( NNH3 = 1)         ! placeholder when not using this set
C       PARAMETER(MXCHNA = 1422)
       PARAMETER(  NNH3 = 4)

C      -----------------
C      For variable HDO
C      -----------------
       INTEGER MXCHND ! max # of channels with HDO pert coefs (2075)
       INTEGER   NHDO ! number of HDO coefficients (4)
       PARAMETER(MXCHND = 1)        ! placeholder when not using this set
C       PARAMETER( NHDO = 1)         ! placeholder when not using this set
C       PARAMETER(MXCHND = 1843)
       PARAMETER(  NHDO = 11)
C
C      ----------------------
C      For non-LTE - placeholder while no coef file
C      ----------------------
       INTEGER MXCNTE ! max # of channels for non-LTE (placeholder 1)
       INTEGER NNCOEF ! # of coefs for non-LTE (placeholder 1)
       INTEGER NTEBOT ! bottom layer for CO2TOP calc
       REAL CO2NTE ! ref CO2 mixing ratio for non-LTE coefs (ppmv)
       PARAMETER(MXCNTE = 264)
       PARAMETER(NNCOEF = 7)
       PARAMETER(NTEBOT = 10)
       PARAMETER(CO2NTE = 400.0)

C      ----------------------
C      For OPTRAN water coefs
C      ----------------------
C      Used in part by modules:
       INTEGER MXCHNW ! max # of channelss with OPTRAN H2O coefs (637)
       INTEGER MXOWLY ! number of OPTRAN water layers
       INTEGER NOWAVG ! # of OPTRAN water average profile values (4)
       INTEGER NH2O   ! number of OPTRAN H2O predictors/coefs (9)
       PARAMETER(MXCHNW = 873)
c       PARAMETER(MXCHNW = 737)
c       PARAMETER(MXCHNW = 865)
c       PARAMETER(MXCHNW = 1602)
C       PARAMETER(MXCHNW = 12)
       PARAMETER(MXOWLY = 300)
       PARAMETER(NOWAVG = 4)
       PARAMETER(  NH2O = 9)           ! was 9
C
C
C      ---------
C      Filenames
C      ---------
       CHARACTER*80 FNCOF1 ! coef set1 
       CHARACTER*80 FNCOF2 ! coef set2 
       CHARACTER*80 FNCOF3 ! coef set3 
       CHARACTER*80 FNCOF4 ! coef set4 
       CHARACTER*80 FNCOF5 ! coef set5 
       CHARACTER*80 FNCOF6 ! coef set6 
       CHARACTER*80 FNCOF7 ! coef set7 
       CHARACTER*80 FNCO2  ! coef co2
       CHARACTER*90 FNOPTR ! coef optran
       CHARACTER*80 FNN2O  ! coef N2O
       CHARACTER*80 FNSO2  ! coef SO2
       CHARACTER*80 FNHNO3 ! coef HNO3
       CHARACTER*80 FNNH3  ! coef NH3
       CHARACTER*80 FNHDO  ! coef HDO
       CHARACTER*80 FNTHER ! coef therm
       CHARACTER*80 FNFX   ! coef fx
       CHARACTER*80 FNPREF ! ref prof
       CHARACTER*80 FNSUN  ! solar data
       CHARACTER*90 FNCOFN ! non-LTE
C
       PARAMETER(FNCOF1='/asl/data/sarta_coef/Data_CrIS_oct16/Coef/set1.dat')
       PARAMETER(FNCOF2='/asl/data/sarta_coef/Data_CrIS_oct16/Coef/set2.dat')
       PARAMETER(FNCOF3='/asl/data/sarta_coef/Data_CrIS_oct16/Coef/set3.dat')
       PARAMETER(FNCOF4='/asl/data/sarta_coef/Data_CrIS_oct16/Coef/set4.dat')
       PARAMETER(FNCOF5='/asl/data/sarta_coef/Data_CrIS_oct16/Coef/set5.dat')
       PARAMETER(FNCOF6='/asl/data/sarta_coef/Data_CrIS_oct16/Coef/set6.dat')
       PARAMETER(FNCOF7='/asl/data/sarta_coef/Data_CrIS_oct16/Coef/set7.dat')
       PARAMETER(FNCO2='/asl/data/sarta_coef/Data_CrIS_oct16/Coef/co2.dat')
       PARAMETER(FNOPTR='/asl/data/sarta_coef/Data_CrIS_oct16/Coef/optran.dat')
       PARAMETER(FNN2O='/asl/data/sarta_coef/Data_CrIS_oct16/Coef/n2o.dat')
       PARAMETER(FNSO2='/asl/data/sarta_coef/Data_CrIS_oct16/Coef/so2.dat')
       PARAMETER(FNHNO3='/asl/data/sarta_coef/Data_CrIS_oct16/Coef/hno3.dat')
       PARAMETER(FNNH3='/asl/data/sarta_coef/Data_CrIS_oct16/Coef/nh3.dat')
       PARAMETER(FNHDO='/asl/data/sarta_coef/Data_CrIS_oct16/Coef/hdo.dat')
       PARAMETER(FNTHER='/asl/data/sarta_coef/Data_CrIS_oct16/Coef/therm.dat')
       PARAMETER(FNCOFN='/asl/data/sarta_coef/Data_CrIS_oct16/Coef/nte_7term.dat')
       PARAMETER(FNFX='/asl/data/sarta_coef/Data_CrIS_oct16/Coef/fx.txt')
       PARAMETER(FNPREF='/asl/data/sarta_coef/Data_CrIS_oct16/Coef/refprof_400_tra')
       PARAMETER(FNSUN='/asl/data/sarta_coef/Data_CrIS_oct16/Solar/sol.txt')

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Mie lookup tables; also see "fnmie.f"
C
c       INTEGER MXMIEA  ! max # of mie particle sizes
c       PARAMETER(MXMIEA = 10) ! ice aggregates=8, all others 10
       INTEGER NMIETY  ! number of mie particle types
       PARAMETER(NMIETY = 3)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Tuning filename
       CHARACTER*80 FNTMLT ! tuning multiplier filename
C
       PARAMETER(FNTMLT='/asl/data/sarta_coef/Data_CrIS_oct16/Coef/tunmlt.txt')
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      ----------------
C      I/O unit numbers
C      ----------------
C      Note: these units are not explicitly openned by the sarta code,
C      they should be set to standard I/O units for your compiler
       INTEGER IOINFO  ! unit number for non-error info messages (6)
       INTEGER IOERR   ! unit number for error messages (2 or 6)
       PARAMETER( IOINFO = 6 )
       PARAMETER( IOERR = 0 )
C
C
C      -----------------
C      Allowed input GUC (Gas Units Code number)
C      -----------------
       INTEGER GUCIN  ! The one & only allowed input GUC number
       PARAMETER( GUCIN = 1 ) ! GUC number for:  molecules/cm^2
c       PARAMETER( GUCIN = 2 ) ! GUC number for:  kilomoles/cm^2
C      Note: GUCIN must be 1 or 2.  All gases in the input RTP
C      must be of this type.

C      End of include file
