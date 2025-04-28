!=======================================================================
!    University of Maryland Baltimore County [UMBC]
!    AIRS
!    RDPROF version with SO2, HNO3, N2O, NH3 trace gases
!F90====================================================================

!ROUTINE NAME:
!    RDPROF

!ABSTRACT:
!    Read in an AIRS FTC formatted profile. Temperature, amounts, etc.

!CALL PROTOCOL:
!    RDPROF ( IPOPN, PFILE, PNAM, ALT, PRES, TEMP, FAMNT, WAMNT, OAMNT,
!       CAMNT, MAMNT, SAMNT, HAMNT, NAMNT, AAMNT )

!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INTEGER   IPOPN    I/O unit number             none
!    CHAR*80   PFILE   filename for desired prof   none

!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL arr  ALT     layer altitudes             m
!    REAL arr  CAMNT   carbon monoxide amount      k.mol/cm2
!    REAL arr  DZ      layer thickness             m
!    REAL arr  FAMNT   fixed gases amount          k.mol/cm2
!    REAL arr  HAMNT   HNO3 amount                 k.mol/cm2
!    REAL arr  MAMNT   CH4 amount                  k.mol/cm2
!    REAL arr  NAMNT   N2O amount                  k.mol/cm2
!    REAL arr  OAMNT   O3 amount                   k.mol/cm2
!    CHAR*40   PNAM    profile name/comment        none
!    REAL arr  SAMNT   SO2 amount                  k.mol/cm2
!    REAL arr  TEMP    temperature                 K
!    REAL arr  WAMNT   water amount                k.mol/cm2
!    REAL arr  AAMNT   ammonia (NH3) amount        k.mol/cm2

!INPUT/OUTPUT PARAMETERS:
!    none

!RETURN VALUES:
!    none

!PARENT(S):
!    USEFAST

!ROUTINES CALLED:
!    none

!FILES ACCESSED:
!    incFTC.f : include file of parameter statements accessed during
!       compilation only.
!    unit IPOPN : input file, ASCII profile file

!COMMON BLOCKS
!    none

!DESCRIPTION:
!    March 1998 version of the 100 layer AIRS Fast Transmittance Code
!    by L.Strow/S.Hannon.

!    An ASCII file containing a profile (in the expected format) is
!    read in, and the relevant temperature and amount profiles are
!    passed back to the calling program.

!    ===================================================================
!    Reads profile data from a text file named PFILE. The data consists
!    of a profile name/description PNAM, followed by rows of data for
!    all 100 layers (in lowest to highest altitude order) consisting of
!    columns:
!       i, Z, dZ, P, T, F, W, O, C, M, S, H, N
!    where:
!       "i"  is a layer number counter (value ignored)
!       "Z"  is the layer average altitude
!       "dZ" is the layer thickness
!       "P"  is the layer slab average pressure PRES
!       "T"  is the layer slab average temperature TEMP (for "fixed")
!       "F"  is the "fixed" (CO2) gases amount
!       "W"  is the water (H2O) amount
!       "O"  is the ozone (O3) amount
!       "C"  is the carbon monoxide (CO) amount
!       "M"  is the methane (CH4) amount
!       "S"  is the sulfur dioxide (SO2) amount
!       "H"  is the nitric acid (HNO3) amount
!       "N"  is the nitrous oxide (N2O) amount
!    ===================================================================

!ALGORITHM REFERENCES:
!    none

!KNOWN BUGS AND LIMITATIONS:
!    none

!ROUTINE HISTORY:
!    Date         Programmer      Comments
!    -----------  --------------  --------------------------------------
!    Dec  1 1994  Scott Hannon    Created; reading of FAMNT disabled
!    Apr 10 1995  Scott Hannon    New header comments; FAMNT enabled;
!                                 added ALT
!    Jun 23 1995  Scott Hannon    Correct some comments
!    Jul  3 1995  Scott Hannon    Added parameter DZ for layer thickness
!     3 Feb 1997  Scott Hannon    Add IPOPN, CAMNT & MAMNT
!    18 May 2005  Scott Hannon    Add HNO3 & N2O based on SO2 code
!    10 May 2018  C Hepplewhite   Add NH3

!END====================================================================

SUBROUTINE RDPROF (IPOPN, PFILE, PNAM, ALT, DZ, PRES, TEMP,  &
    FAMNT, WAMNT, OAMNT, CAMNT, MAMNT, SAMNT, HAMNT, NAMNT, AAMNT)
!      =================================================================

!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
use incFTC

!-----------------------------------------------------------------------
IMPLICIT NONE
!-----------------------------------------------------------------------

INTEGER, INTENT(IN)                      :: IPOPN
CHARACTER (LEN=90), INTENT(IN)           :: PFILE
CHARACTER (LEN=40), INTENT(OUT)          :: PNAM
REAL, INTENT(IN OUT)                     :: ALT(MAXLAY)
REAL, INTENT(IN OUT)                     :: DZ(MAXLAY)
REAL, INTENT(IN OUT)                     :: PRES(MAXLAY)
REAL, INTENT(IN OUT)                     :: TEMP(MAXLAY)
REAL, INTENT(IN OUT)                     :: FAMNT(MAXLAY)
REAL, INTENT(IN OUT)                     :: WAMNT(MAXLAY)
REAL, INTENT(IN OUT)                     :: OAMNT(MAXLAY)
REAL, INTENT(IN OUT)                     :: CAMNT(MAXLAY)
REAL, INTENT(IN OUT)                     :: MAMNT(MAXLAY)
REAL, INTENT(IN OUT)                     :: SAMNT(MAXLAY)
REAL, INTENT(IN OUT)                     :: HAMNT(MAXLAY)
REAL, INTENT(IN OUT)                     :: NAMNT(MAXLAY)
REAL, INTENT(IN OUT)                     :: AAMNT(MAXLAY)

!-----------------------------------------------------------------------
!      EXTERNAL FUNCTIONS
!-----------------------------------------------------------------------
!      none

!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
!      Input

!      Output

!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
INTEGER :: IERR
INTEGER :: IJUNK
INTEGER :: L
CHARACTER (LEN=80) :: CLINE

!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none

!***********************************************************************
!***********************************************************************
!                    EXECUTABLE CODE follows...
!***********************************************************************
!***********************************************************************

!      ---------------------
!      Open the profile file
!      ---------------------
OPEN(UNIT=IPOPN,FILE=PFILE,STATUS='OLD',FORM='FORMATTED', IOSTAT=IERR)
IF (IERR /= 0) THEN
  WRITE(6,1010) IERR, PFILE
  1010     FORMAT('Error ',I5,' opening profile file:',/,A80)
  STOP
END IF

!      ----------------------------------------
!      Skip any comments at the top of the file
!      ----------------------------------------
10    READ(IPOPN,9000) CLINE
9000  FORMAT(A80)
IF (CLINE(1:1) == '!') THEN
  GO TO 10
ELSE
  BACKSPACE(IPOPN)
END IF

!      -------------------------------
!      Read the profile's name/comment
!      -------------------------------
READ(IPOPN,9010) PNAM
9010  FORMAT(A40)
!       WRITE(6,*) PNAM

!      --------------------------------------------------
!      Read in the temperature and amounts for each layer
!      --------------------------------------------------
!      Note: read the layers in reverse order.
DO L=MAXLAY,1,-1
!         Layer number, altitude, thickness, pressure, temperature,
!         fixed, H2O, O3, CO, and CH4 amounts
  READ(IPOPN,*) IJUNK, ALT(L), DZ(L), PRES(L), TEMP(L),  &
      FAMNT(L), WAMNT(L), OAMNT(L), CAMNT(L), MAMNT(L),  &
      SAMNT(L), HAMNT(L), NAMNT(L), AAMNT(L)
  ENDDO
    
!      ----------------------
!      Close the profile file
!      ----------------------
    CLOSE(IPOPN)
    
    RETURN
    END SUBROUTINE RDPROF

