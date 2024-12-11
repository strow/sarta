!=======================================================================
 
! Code converted using TO_F90_LOOP by Alan Miller
! Date: 2023-04-04  Time: 16:44:55
 
!=======================================================================

!    University of Maryland Baltimore County [UMBC]

!    AIRS (Atmospheric Infra-Red Sounder)

!    incFTC

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
!    Include file for the December 2005 100 layer AIRS fast
!    Stand Alone RTA (SARTA) code by L.L.Strow/S.Hannon.

!    Parameter statements for the FTC routines.


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    none


!ROUTINE HISTORY:
! Date        Programmer     Comments
! ----------- -------------- -------------------------------------------
!  1 Dec 1994 Scott Hannon   Created
! 31 Jan 1997 Scott Hannon   Re-wrote for FWO+FOW+FMW+FCOW=Feb97 FTC
!  3 Sep 1997 Scott Hannon   Re-wrote for sets 1 - 7
! 30 Sep 1997 Scott Hannon   Added NCO2 and MXCHNC
! 26 Feb 1998 Scott Hannon   Added OPTRAN variables for water, and
!                            changed both N1H2O & N3H2O from 13 to 11
! 23 Sep 1999 Scott Hannon   Change number of channel dimensions for
!                            new Sep99 version of fast model.
!  5 Apr 2000 Scott Hannon   Added MXEMIS
!  4 Aug 2000 Scott Hannon   Changes values for use with testfast
! 11 Aug 2000 Scott Hannon   Change from 4 to 5 term H2O continuum
! 23 Jan 2001 Scott Hannon   Update values of C1 & C2
! 15 Feb 2001 Scott Hannon   Add MAXPRO, CO2STD, IOERR, IOINFO,
!                            MXGAS, CSARTA, and all filenames
! 24 Apr 2001 Scott Hannon   Add MXMIEA and FNMIEA
! 14 Aug 2001 Scott Hannon   Add FNMIEE and FNMIEG
! 21 Nov 2001 Scott Hannon   Add VSARTA, VSCOEF, & VCLOUD; remove CSARTA
! 12 Sep 2002 Scott Hannon   Updated for m135f (-13.5 um with fringes)
! 17 Dec 2002 Scott Hannon   Updated for revised(Dec02) m135f
!  3 Jan 2003 Scott Hannon   Updated VSARTA for version 1.04
!  3 Feb 2004 Scott Hannon   Updated for "cc2"
! 19 Feb 2004 Scott Hannon   Add FNTMLT & update VSARTA for v1.05
! 07 Apr 2005 Scott Hannon   NFCOEF increased from 5 to 6 for v1.06
! 18 May 2005 Scott Hannon   update for HNO3 version
! 29 Jun 2005 Scott Hannon   "trace" version v1.07 with CO2,SO2,HNO3,N2O
! 13 Oct 2005 Scott Hannon   Add variables for non-LTE
! 22 Nov 2005 Scott Hannon   Replace set1,set2,CO2 coefs for new M12
! 30 Mar 2006 Scott Hannon   Change from separate MI#* for each cloud
!                            type to a single MIE* 3-D array for all
!                            types, and add MIETYP & NMIETY.
! 02 May 2007 Scott Hannon   Added XSALT
! 12 May 2009 Scott Hannon   Add VTUNNG; delete VCLOUD


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

CHARACTER (LEN=40) :: VSARTA  ! SARTA source code version
CHARACTER (LEN=40) :: VSCOEF  ! SARTA coefficient version
CHARACTER (LEN=40) :: VTUNNG  ! optical depth tuning version
!      version template    '#.## YYYY-MM-DD <--------comment------->'
PARAMETER( VSARTA = '1.08 2010-09-14 rtpV201 PCLSAM slab HG3')
PARAMETER( VSCOEF = 'AIRS 2008-04-30 XCSNAMEX CO2=385' )
PARAMETER( VTUNNG = 'v6 standard; refprof N2O x1/1.04')

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

REAL :: CO2STD ! standard CO2 PPMV mixing ratio (370)
!       PARAMETER( CO2STD = 370.0 )  ! m130x, m140x
PARAMETER( CO2STD = 385.0 )  ! m130, m140, m150

REAL :: XSALT ! expected nominal satellite altitude (km)
PARAMETER( XSALT = 705.0 )


!      -----------------------------------
!      Channels and layers other variables
!      -----------------------------------
INTEGER :: MAXLAY ! # of layers (100)
INTEGER :: NSET ! # of coefficient data sets (7)
INTEGER :: MXCHAN ! max total # of channels (2378)
INTEGER :: NFCOEF ! # of downwelling thermal "F" factor coefs
INTEGER :: MXEMIS ! max # of input emis/rho data points
INTEGER :: MAXPRO ! max # of user specified profiles
INTEGER :: MXGAS ! max # of gases in user profile
PARAMETER(MAXLAY = 100)
PARAMETER(  NSET = 7)
PARAMETER(MXCHAN = 2834)
PARAMETER(NFCOEF = 6)
PARAMETER(MXEMIS = 100)
PARAMETER(MAXPRO = 25)
PARAMETER( MXGAS = 44)



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
INTEGER :: MXCHN2 ! max # of channels for set2 = FOW  (325)
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
INTEGER :: MXCHN3 ! max # of channels for set3 = FMW  (396)
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
INTEGER :: MXCHN4 ! max # of channels for set4 = FCOW (85)
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
INTEGER :: MXCHN5 ! max # of channels for set5 = BFSW (210)
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
INTEGER :: MXCHN6 ! max # of channels for set6 = MFMW (217)
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
INTEGER :: MXCHN7 ! max # of channels for set7 = MFBW (140)
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
INTEGER :: MXCHNC ! max # of channels with CO2 pert coefs (1082)
INTEGER :: NCO2 ! number of CO2 coefficients
PARAMETER(MXCHNC = 1082)
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
INTEGER :: MXCHNN ! max # of channels with N2O pert coefs (586)
INTEGER :: NN2O ! number of N2O coefficients
PARAMETER(MXCHNN = 586)
PARAMETER(  NN2O = 7)


!      ----------------------
!      For OPTRAN water coefs
!      ----------------------
!      Used in part by modules:
INTEGER :: MXCHNW ! max # of channelss with OPTRAN H2O coefs (754)
INTEGER :: MXOWLY ! number of OPTRAN water layers
INTEGER :: NOWAVG ! # of OPTRAN water average profile values (4)
INTEGER :: NH2O   ! number of OPTRAN H2O predictors/coefs (9)
PARAMETER(MXCHNW = 754)
PARAMETER(MXOWLY = 300)
PARAMETER(NOWAVG = 4)
PARAMETER(  NH2O = 9)


!      -----------
!      For non-LTE
!      -----------
INTEGER :: MXCNTE ! max # of channels for non-LTE (203)
INTEGER :: NNCOEF ! # of coefs for non-LTE
INTEGER :: NTEBOT ! bottom layer for CO2TOP calc
REAL :: CO2NTE ! ref CO2 mixing ratio for non-LTE coefs (ppmv)
PARAMETER(MXCNTE = 203)
PARAMETER(NNCOEF = 7)
PARAMETER(NTEBOT = 10)
PARAMETER(CO2NTE = 370.0)



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
CHARACTER (LEN=80) :: FNN2O  ! coef N2O
CHARACTER (LEN=80) :: FNOPTR ! coef optran
CHARACTER (LEN=80) :: FNTHER ! coef therm
CHARACTER (LEN=80) :: FNCOFN ! non-LTE
CHARACTER (LEN=80) :: FNFX   ! coef fx
CHARACTER (LEN=80) :: FNPREF ! ref prof
CHARACTER (LEN=80) :: FNSUN  ! solar data

PARAMETER(FNCOF1= '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'  &
    // 'set1_XCSNAMEX.dat')
PARAMETER(FNCOF2= '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'  &
    // 'set2_XCSNAMEX.dat')
PARAMETER(FNCOF3= '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'  &
    // 'set3_XCSNAMEX.dat')
PARAMETER(FNCOF4= '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'  &
    // 'set4_XCSNAMEX.dat')
PARAMETER(FNCOF5= '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'  &
    // 'set5_XCSNAMEX.dat')
PARAMETER(FNCOF6= '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'  &
    // 'set6_XCSNAMEX.dat')
PARAMETER(FNCOF7= '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'  &
    // 'set7_XCSNAMEX.dat')
PARAMETER(FNCO2 = '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'  &
    // 'CO2_5term_XCSNAMEX.dat')
PARAMETER(FNSO2 = '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'  &
    // 'SO2_XCSNAMEX.dat')
PARAMETER(FNHNO3 = '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'  &
    // 'HNO3_XCSNAMEX.dat')
PARAMETER(FNN2O = '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'  &
    // 'N2O_XCSNAMEX.dat')
PARAMETER(FNOPTR= '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'  &
    // 'optran_XCSNAMEX.dat')
PARAMETER(FNTHER= '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'  &
    // 'therm_XCSNAMEX.dat')
PARAMETER(FNCOFN= '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'  &
    // 'nonLTE7_XCSNAMEX.dat')
PARAMETER(FNFX  = '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'  &
    // 'fx.txt')
PARAMETER(FNPREF= '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
!     $ // 'profref_trace385')
!     CHANGE below to profref_trace385tuned for compatability with ASL sarta_cloudy
!     refprof N2O scaled by 1/1.04 for tuning purposes  &
// 'profref_trace385')
PARAMETER(FNSUN = '/asl/data/sarta_database/Data_AIRS_apr08/Solar/'  &
    // 'solar_XCSNAMEX.txt')

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Mie lookup tables; also see "fnmie.f"

INTEGER :: MXMIEA  ! max # of mie particle sizes
INTEGER :: NMIETY  ! number of mie particle types
PARAMETER(MXMIEA = 10) ! ice aggregates=8, all others 10
PARAMETER(NMIETY = 3)

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Tuning filename
CHARACTER (LEN=80) :: FNTMLT ! tuning multiplier filename

PARAMETER(FNTMLT= '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'  &
    // 'apr2016_tunmlt_wcon_nte.txt')

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

!      End of include file
