C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore Country (UMBC)
C
C    CRIS HiRes
C
C    FNMIE
C
!F77====================================================================


!ROUTINE NAME:
C    FNMIE


!ABSTRACT:0
C    Set Mie table filenames; this is a work-around for FORTRAN 77's
C    inability to assign an array of filenames in an include file
C    using PARAMETER.  This routine is essentially the executable
C    equivant of an include file containing a list of filenames.
C
C    Create/edit a new version of this file for every set of Mie
C    files you wish to use.  The number of entries (NMIETY) must
C    match the value in the "incFTC.f" include file.


!CALL PROTOCOL
C    FNMIE( VCLOUD, MIETYP, FNMIEA, FNMIEE, FNMIEG )


!INPUT PARAMETERS:
C    none


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    CHAR*240  VCLOUD  cloud version string        none
C    INT arr   MIETYP  particle type code number   none
C    CHAR arr  FNMIEA  absorption filenames        none
C    CHAR arr  FNMIEE  extinction filenames        none
C    CHAR arr  FNMIEG  "g" asymmetry filenames     none


!INPUT/OUTPUT PARAMETERS:
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    RDCOEF


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    incFTC.f : include file of parameter statements accessed during
C       compilation only.


!COMMON BLOCKS
C    none


!DESCRIPTION:
C    May 2009 version of the SARTA v1.08 code with PCLSAM Fast
C    Transmittance Code by S.Hannon.
C
C    Copies individual filenames into an array of filenames.
C
C    Recommended Mie particle type code numbers (08 Jan 2007)
C    Min - Max  Description
C    ---------  --------------------------------------------------------
C    000 - 099  Black clouds
C    100 - 199  Spherical liquid H2O droplets
C    200 - 299  Ice aggregates
C    300 - 399  Dust/mineral
C    400 - 499  Sea salt (liquid H2O + salt)
C    500 - 599  Smoke/soot
C    600 - 699  Sulfate/pollutants


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C Date        Programmer     Comments
C ----------- -------------- -------------------------------------------
C 24 Apr 2006 Scott Hannon   Created
C 08 Jan 2007 Scott Hannon   Change MIETYP code number values
C 12 May 2009 Scott Hannon   Add VCLOUD & CLDSTR


!END====================================================================

C      =================================================================
       SUBROUTINE FNMIE ( VCLOUD, MIETYP, FNMIEA, FNMIEE, FNMIEG )

C      =================================================================


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
C      Output
       CHARACTER*240 VCLOUD
       INTEGER MIETYP(NMIETY)
       CHARACTER*79 FNMIEA(NMIETY)
       CHARACTER*79 FNMIEE(NMIETY)
       CHARACTER*79 FNMIEG(NMIETY)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER I
       INTEGER IS
       INTEGER IE
       INTEGER LENNB
       INTEGER N
       CHARACTER*40 CLDSTR(NMIETY)


C-----------------------------------------------------------------------
C      SAVE STATEMENTS
C-----------------------------------------------------------------------
C      none


C***********************************************************************
C***********************************************************************
C                    EXECUTABLE CODE
C***********************************************************************
C***********************************************************************
C
C      Make sure NMIETY is as this routine expects
       IF (NMIETY .NE. 3) THEN
          WRITE(IOERR,1010) NMIETY,3
 1010     FORMAT('incFTC.f NMIETY=',I3,' but fnmie.f expects ',I3)
       ENDIF

C      mie particles #1: PingYang/BryanBaum GeneralHabitMixture
C                 123456789012345678901234567890123456789
C                 <------------cloud string------------->
       CLDSTR(1)='201=ice_general habit 2013-07-25 Baum'
       MIETYP(1)=201
       FNMIEA(1)=
     $ '/asl/rta/sarta_coef/Data_CrIS_oct16/Mie/ice_baumGHM_abs_cris_hires_g4.dat'
       FNMIEE(1)=
     $ '/asl/rta/sarta_coef/Data_CrIS_oct16/Mie/ice_baumGHM_ext_cris_hires_g4.dat'
       FNMIEG(1)=
     $ '/asl/rta/sarta_coef/Data_CrIS_oct16/Mie/ice_baumGHM_asy_cris_hires_g4.dat'
C

C      mie particles #2: water drop
C                 <------------cloud string------------->
       CLDSTR(2)='101=waterdrop modis params 2008-06-13'
       MIETYP(2)=101
       FNMIEA(2)=
     $ '/asl/rta/sarta_coef/Data_CrIS_oct16/Mie/water_modisVEFF_abs_cris_hires_g4.dat'
       FNMIEE(2)=
     $ '/asl/rta/sarta_coef/Data_CrIS_oct16/Mie/water_modisVEFF_ext_cris_hires_g4.dat'
       FNMIEG(2)=
     $ '/asl/rta/sarta_coef/Data_CrIS_oct16/Mie/water_modisVEFF_asy_cris_hires_g4.dat'
C

C      mie particles #3: desert dust
C                 <------------cloud string------------->
       CLDSTR(3)='301=volz 20??-06-13'
       MIETYP(3)=301
       FNMIEA(3)=
     $ '/asl/rta/sarta_coef/Data_CrIS_oct16/Mie/volz_1_0507_log2_abs_cris_hires_g4.dat'
       FNMIEE(3)=
     $ '/asl/rta/sarta_coef/Data_CrIS_oct16/Mie/volz_1_0507_log2_ext_cris_hires_g4.dat'
       FNMIEG(3)=
     $ '/asl/rta/sarta_coef/Data_CrIS_oct16/Mie/volz_1_0507_log2_asy_cris_hires_g4.dat'
C

C      Assign VCLOUD string
       IE=0
       DO I=1,NMIETY
          N=LENNB(CLDSTR(I))
          IS=IE + 1
          IE=IS + N - 1
          VCLOUD(IS:IE)=CLDSTR(I)
          IE=IE + 1
          IF (I .EQ. NMIETY) THEN
             VCLOUD(IE:IE)=CHAR(0)
          ELSE
             VCLOUD(IE:IE)=';'
          ENDIF
       ENDDO
C
       RETURN
       END
