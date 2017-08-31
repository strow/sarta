C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore Country (UMBC)
C
C    AIRS
C
C    SETEMS
C
!F77====================================================================


!ROUTINE NAME:
C    SETEMS

!ABSTRACT:
C    Assign the emissivity and reflectivity data for every channel
C    being used.


!CALL PROTOCOL
C    RDEMIS ( NCHAN, NEMIS, FREQ, FEMIS, XEMIS, XRHO,
C       LRHOT, EMIS, RHOSUN, RHOTHR )


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER   NCHAN   number of channels          none
C    INTEGER   NEMIS   number of emis pts          none
C    REAL arr  FREQ    channel frequencies         cm^-1
C    REAL arr  FEMIS   raw emis freq points        cm^-1
C    REAL arr  XEMIS   raw emis points             none (0 to 1)
C    REAL arr  XRHO    raw reflec points           1/steradian
C    LOGICAL   LRHOT   force refl therm rho?       none

!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    REAL arr  EMIS    surface emissivity          none
C    REAL arr  RHOSUN  reflectivity for solar      1/steradian
C    REAL arr  RHOTHR  reflectivity for thermal    1/steradian


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


!COMMON BLOCKS
C    none


!DESCRIPTION:
C    August 2000 version of the 100 layer AIRS Fast Transmittance
C    Code by L.Strow/S.Hannon.
C
C    Loops over the passed in EMIS & RHO points and interpolates
C    or extrapolates them onto the channel freqs being used.
C    Interpolations are linear in wavelength (not frequency).


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C Date        Programmer     Comments
C ----------- -------------- -------------------------------------------
C 14 Feb 2001 Scott Hannon   Created based on rdemis.f
C 06 Feb 2004 Scott Hannon   Add LRHOT argument and associated code
C 24 Oct 2008 Scott Hannon   Update for rtpV201: input emis & rho is
C                               now specified with same freq points
C 25 Nov 2008 Scott Hannon   Update for cloudy SARTA rtpV201 with
C                               spectral cemis & crho for 2 clouds

!END====================================================================

C      =================================================================
       SUBROUTINE SETEMS ( NCHAN, NEMIS, FREQ, FEMIS, XEMIS,
     $    XRHO, XCEMI1, XCRHO1, XCEMI2, XCRHO2, LRHOT,
     $    EMIS, RHOSUN, RHOTHR, CEMIS1, CRHOS1, CRHOT1,
     $    CEMIS2, CRHOS2, CRHOT2 )
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
C      Input
       INTEGER  NCHAN
       INTEGER  NEMIS
       REAL   FREQ(MXCHAN)
       REAL  FEMIS(MXEMIS)
       REAL  XEMIS(MXEMIS)
       REAL   XRHO(MXEMIS)
       REAL XCEMI1(MXEMIS)
       REAL XCRHO1(MXEMIS)
       REAL XCEMI2(MXEMIS)
       REAL XCRHO2(MXEMIS)
       LOGICAL  LRHOT
C
C      Output
       REAL   EMIS(MXCHAN)
       REAL RHOSUN(MXCHAN)
       REAL RHOTHR(MXCHAN)
       REAL CEMIS1(MXCHAN)
       REAL CRHOS1(MXCHAN)
       REAL CRHOT1(MXCHAN)
       REAL CEMIS2(MXCHAN)
       REAL CRHOS2(MXCHAN)
       REAL CRHOT2(MXCHAN)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER      I  ! generic looping variable
       INTEGER      J  ! generic looping variable
       INTEGER SORTED  ! conrol flag for sorts
       REAL DELELM(MXEMIS)  ! delta for emis wavelength
       REAL DELEMS(MXEMIS)  ! delta for emis
       REAL DELRHO(MXEMIS)  ! delta for rho
       REAL DELCE1(MXEMIS)  ! delta for cemis1
       REAL DELCR1(MXEMIS)  ! delta for crho1
       REAL DELCE2(MXEMIS)  ! delta for cemis2
       REAL DELCR2(MXEMIS)  ! delta for crho2
       REAL     DX          ! delta X
       REAL   ELAM(MXEMIS)  ! emis wavelength
       REAL RJUNK1  ! generic junk/work variable
       REAL RJUNK2  ! generic junk/work variable
       REAL RJUNK3  ! generic junk/work variable
       REAL RJUNK4  ! generic junk/work variable
       REAL RJUNK5  ! generic junk/work variable
       REAL RJUNK6  ! generic junk/work variable
       REAL RJUNK7  ! generic junk/work variable


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
C      ---------------------------
C      Sort the emis & rho by freq
C      ---------------------------
       SORTED=1
 10    IF (SORTED .EQ. 1) THEN
          SORTED=0
          DO I=1,NEMIS-1
             IF (FEMIS(I) .GT. FEMIS(I+1)) THEN
                RJUNK1=FEMIS(I)
                RJUNK2=XEMIS(I)
                RJUNK3=XRHO(I)
                RJUNK4=XCEMI1(I)
                RJUNK5=XCRHO1(I)
                RJUNK6=XCEMI2(I)
                RJUNK7=XCRHO2(I)
                FEMIS(I)=FEMIS(I+1)
                XEMIS(I)=XEMIS(I+1)
                XRHO(I)=XRHO(I+1)
                FEMIS(I+1)=RJUNK1
                XEMIS(I+1)=RJUNK2
                XRHO(I+1)=RJUNK3
                XCEMI1(I+1)=RJUNK4
                XCRHO1(I+1)=RJUNK5
                XCEMI2(I+1)=RJUNK6
                XCRHO2(I+1)=RJUNK7
                SORTED=1
             ENDIF
          ENDDO
          GOTO 10
       ENDIF
C
C      ----------------------------------------
C      Calc wavelength and interpolation deltas
C      ----------------------------------------
       ELAM(1)=1.0E+4/FEMIS(1) ! wavelength in microns
       DO J=2,NEMIS
          ELAM(J)=1.0E+4/FEMIS(J)
          DELELM(J-1)=ELAM(J) - ELAM(J-1)
          DELEMS(J-1)=XEMIS(J) - XEMIS(J-1)
          DELRHO(J-1)=XRHO(J) - XRHO(J-1)
          DELCE1(J-1)=XCEMI1(J) - XCEMI1(J-1)
          DELCR1(J-1)=XCRHO1(J) - XCRHO1(J-1)
          DELCE2(J-1)=XCEMI2(J) - XCEMI2(J-1)
          DELCR2(J-1)=XCRHO2(J) - XCRHO2(J-1)
       ENDDO
C
C      ------------------------------------
C      Interpolate emis & rho onto channels
C      ------------------------------------
C      Loop over channels (note: chan freqs may be in any order)
       DO I=1,NCHAN
C
C         Emissivity
          IF ( FREQ(I) .LE. FEMIS(1) ) THEN
             EMIS(I)=XEMIS(1)
             RHOSUN(I)=XRHO(1)
             CEMIS1(I)=XCEMI1(1)
             CRHOS1(I)=XCRHO1(1)
             CEMIS2(I)=XCEMI2(1)
             CRHOS2(I)=XCRHO2(1)
          ELSEIF ( FREQ(I) .GE. FEMIS(NEMIS) ) THEN
             EMIS(I)=XEMIS(NEMIS)
             RHOSUN(I)=XRHO(NEMIS)
             CEMIS1(I)=XCEMI1(NEMIS)
             CRHOS1(I)=XCRHO1(NEMIS)
             CEMIS2(I)=XCEMI2(NEMIS)
             CRHOS2(I)=XCRHO2(NEMIS)
          ELSE
C            Determine the index of the upper bounding FEMIS
             J=2
 20          IF ( FEMIS(J) .LT. FREQ(I) ) THEN
                J=J + 1
                GOTO 20
             ENDIF
C            Convert to lower boundary index
             J=J - 1
C
             DX=(( 1.0E+4/FREQ(I) - ELAM(J) )/DELELM(J))
             EMIS(I) = XEMIS(J) + DX*DELEMS(J)
             RHOSUN(I) = XRHO(J) + DX*DELRHO(J)
             CEMIS1(I) = XCEMI1(J) + DX*DELCE1(J)
             CRHOS1(I) = XCRHO1(J) + DX*DELCR1(J)
             CEMIS2(I) = XCEMI2(J) + DX*DELCE2(J)
             CRHOS2(I) = XCRHO2(J) + DX*DELCR2(J)
          ENDIF
C
          IF (LRHOT) THEN
C            Force reflected thermal rho = (1-emis)/pi
             RHOTHR(I)=(1.0 - EMIS(I))/PI
             CRHOT1(I)=(1.0 - CEMIS1(I))/PI
             CRHOT2(I)=(1.0 - CEMIS2(I))/PI
          ELSE
C            Reflected thermal uses the same rho as solar
             RHOTHR(I)=RHOSUN(I)
             CRHOT1(I)=CRHOS1(I)
             CRHOT2(I)=CRHOS1(I)
          ENDIF
C
       ENDDO

       RETURN
       END
