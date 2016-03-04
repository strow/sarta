C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore County [UMBC]
C
C    AIRS
C
C    RDPROF version with SO2, HNO3, & N2O trace gases
C
!F77====================================================================


!ROUTINE NAME:
C    RDPROF


!ABSTRACT:
C    Read in an AIRS FTC formatted profile. Temperature, amounts, etc.


!CALL PROTOCOL:
C    RDPROF ( IOUN, PFILE, PNAM, ALT, PRES, TEMP, FAMNT, WAMNT, OAMNT,
C       CAMNT, MAMNT, SAMNT, HAMNT, NAMNT )


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER   IOUN    I/O unit number             none
C    CHAR*80   PFILE   filename for desired prof   none


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    REAL arr  ALT     layer altitudes             m
C    REAL arr  CAMNT   carbon monoxide amount      k.mol/cm2
C    REAL arr  DZ      layer thickness             m
C    REAL arr  FAMNT   fixed gases amount          k.mol/cm2
C    REAL arr  HAMNT   HNO3 amount                 k.mol/cm2
C    REAL arr  MAMNT   CH4 amount                  k.mol/cm2
C    REAL arr  NAMNT   N2O amount                  k.mol/cm2
C    REAL arr  OAMNT   O3 amount                   k.mol/cm2
C    CHAR*40   PNAM    profile name/comment        none
C    REAL arr  SAMNT   SO2 amount                  k.mol/cm2
C    REAL arr  TEMP    temperature                 K
C    REAL arr  WAMNT   water amount                k.mol/cm2


!INPUT/OUTPUT PARAMETERS:
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    USEFAST


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    incFTC.f : include file of parameter statements accessed during
C       compilation only.
C    unit IOUN : input file, ASCII profile file


!COMMON BLOCKS
C    none


!DESCRIPTION:
C    March 1998 version of the 100 layer AIRS Fast Transmittance Code
C    by L.Strow/S.Hannon.
C
C    An ASCII file containing a profile (in the expected format) is
C    read in, and the relevant temperature and amount profiles are
C    passed back to the calling program.
C
C    ===================================================================
C    Reads profile data from a text file named PFILE. The data consists
C    of a profile name/description PNAM, followed by rows of data for
C    all 100 layers (in lowest to highest altitude order) consisting of
C    columns:
C       i, Z, dZ, P, T, F, W, O, C, M, S, H, N
C    where:
C       "i"  is a layer number counter (value ignored)
C       "Z"  is the layer average altitude
C       "dZ" is the layer thickness
C       "P"  is the layer slab average pressure PRES
C       "T"  is the layer slab average temperature TEMP (for "fixed")
C       "F"  is the "fixed" (CO2) gases amount
C       "W"  is the water (H2O) amount
C       "O"  is the ozone (O3) amount
C       "C"  is the carbon monoxide (CO) amount
C       "M"  is the methane (CH4) amount
C       "S"  is the sulfur dioxide (SO2) amount
C       "H"  is the nitric acid (HNO3) amount
C       "N"  is the nitrous oxide (N2O) amount
C    ===================================================================


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date         Programmer      Comments
C    -----------  --------------  --------------------------------------
C    Dec  1 1994  Scott Hannon    Created; reading of FAMNT disabled
C    Apr 10 1995  Scott Hannon    New header comments; FAMNT enabled;
C                                 added ALT
C    Jun 23 1995  Scott Hannon    Correct some comments
C    Jul  3 1995  Scott Hannon    Added parameter DZ for layer thickness
C     3 Feb 1997  Scott Hannon    Add IOUN, CAMNT & MAMNT
C    18 May 2005  Scott Hannon    Add HNO3 & N2O based on SO2 code


!END====================================================================

C      =================================================================
       SUBROUTINE RDPROF (IOUN, PFILE, PNAM, ALT, DZ, PRES, TEMP,
     $    FAMNT, WAMNT, OAMNT, CAMNT, MAMNT, SAMNT, HAMNT, NAMNT)
C      =================================================================
C
C-----------------------------------------------------------------------
C      IMPLICIT NONE
C-----------------------------------------------------------------------
       IMPLICIT NONE


C-----------------------------------------------------------------------
C      INCLUDE FILES
C-----------------------------------------------------------------------
       include 'incFTC.f'


C-----------------------------------------------------------------------
C      EXTERNAL FUNCTIONS
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      Input
       INTEGER   IOUN
       CHARACTER*80  PFILE
C
C      Output
       CHARACTER*40   PNAM
       REAL    ALT(MAXLAY)
       REAL     DZ(MAXLAY)
       REAL   PRES(MAXLAY)
       REAL   TEMP(MAXLAY)
       REAL  FAMNT(MAXLAY)
       REAL  WAMNT(MAXLAY)
       REAL  OAMNT(MAXLAY)
       REAL  CAMNT(MAXLAY)
       REAL  MAMNT(MAXLAY)
       REAL  SAMNT(MAXLAY)
       REAL  HAMNT(MAXLAY)
       REAL  NAMNT(MAXLAY)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER   IERR
       INTEGER  IJUNK
       INTEGER      L
       CHARACTER*80  CLINE


C-----------------------------------------------------------------------
C      SAVE STATEMENTS
C-----------------------------------------------------------------------
C      none


C***********************************************************************
C***********************************************************************
C                    EXECUTABLE CODE follows...
C***********************************************************************
C***********************************************************************
C
C      ---------------------
C      Open the profile file
C      ---------------------
       OPEN(UNIT=IOUN,FILE=PFILE,STATUS='OLD',FORM='FORMATTED',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1010) IERR, PFILE
 1010     FORMAT('Error ',I5,' openning profile file:',/,A80)
          STOP
       ENDIF
C
C      ----------------------------------------
C      Skip any comments at the top of the file
C      ----------------------------------------
 10    READ(IOUN,9000) CLINE
 9000  FORMAT(A80)
       IF (CLINE(1:1) .EQ. '!') THEN
          GOTO 10
       ELSE
          BACKSPACE(IOUN)
       ENDIF
C
C      -------------------------------
C      Read the profile's name/comment
C      -------------------------------
       READ(IOUN,9010) PNAM
 9010  FORMAT(A40)
C
C      --------------------------------------------------
C      Read in the temperature and amounts for each layer
C      --------------------------------------------------
C      Note: read the layers in reverse order.
       DO L=MAXLAY,1,-1
C         Layer number, altitude, thickness, pressure, temperature,
C         fixed, H2O, O3, CO, and CH4 amounts
          READ(IOUN,*) IJUNK, ALT(L), DZ(L), PRES(L), TEMP(L),
     $       FAMNT(L), WAMNT(L), OAMNT(L), CAMNT(L), MAMNT(L),
     $       SAMNT(L), HAMNT(L), NAMNT(L)
       ENDDO
C
C      ----------------------
C      Close the profile file
C      ----------------------
       CLOSE(IOUN)
C
       RETURN
       END
