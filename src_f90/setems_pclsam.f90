!=======================================================================
 
! Code converted using TO_F90_LOOP by Alan Miller
! Date: 2023-04-04  Time: 16:44:56
 
!=======================================================================

!    University of Maryland Baltimore Country (UMBC)

!    AIRS

!    SETEMS

!F77====================================================================


!ROUTINE NAME:
!    SETEMS

!ABSTRACT:
!    Assign the emissivity and reflectivity data for every channel
!    being used.


!CALL PROTOCOL
!    RDEMIS ( NCHAN, NEMIS, FREQ, FEMIS, XEMIS, XRHO,
!       LRHOT, EMIS, RHOSUN, RHOTHR )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INTEGER   NCHAN   number of channels          none
!    INTEGER   NEMIS   number of emis pts          none
!    REAL arr  FREQ    channel frequencies         cm^-1
!    REAL arr  FEMIS   raw emis freq points        cm^-1
!    REAL arr  XEMIS   raw emis points             none (0 to 1)
!    REAL arr  XRHO    raw reflec points           1/steradian
!    LOGICAL   LRHOT   force refl therm rho?       none

!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL arr  EMIS    surface emissivity          none
!    REAL arr  RHOSUN  reflectivity for solar      1/steradian
!    REAL arr  RHOTHR  reflectivity for thermal    1/steradian


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


!COMMON BLOCKS
!    none


!DESCRIPTION:
!    August 2000 version of the 100 layer AIRS Fast Transmittance
!    Code by L.Strow/S.Hannon.

!    Loops over the passed in EMIS & RHO points and interpolates
!    or extrapolates them onto the channel freqs being used.
!    Interpolations are linear in wavelength (not frequency).


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    none


!ROUTINE HISTORY:
! Date        Programmer     Comments
! ----------- -------------- -------------------------------------------
! 14 Feb 2001 Scott Hannon   Created based on rdemis.f
! 06 Feb 2004 Scott Hannon   Add LRHOT argument and associated code
! 24 Oct 2008 Scott Hannon   Update for rtpV201: input emis & rho is
!                               now specified with same freq points
! 25 Nov 2008 Scott Hannon   Update for cloudy SARTA rtpV201 with
!                               spectral cemis & crho for 2 clouds

!END====================================================================

!      =================================================================

MODULE SET_EMS_PCLSAM

IMPLICIT NONE

CONTAINS


SUBROUTINE SETEMS ( NCHAN, NEMIS, FREQ, FEMIS, XEMIS,  &
    XRHO, XCEMI1, XCRHO1, XCEMI2, XCRHO2, LRHOT,  &
    EMIS, RHOSUN, RHOTHR, CEMIS1, CRHOS1, CRHOT1, CEMIS2, CRHOS2, CRHOT2 )
!      =================================================================


!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
INCLUDE 'incFTC.f90'


INTEGER, INTENT(IN)                      :: NCHAN
INTEGER, INTENT(IN)                      :: NEMIS
REAL, INTENT(IN)                         :: FREQ(MXCHAN)
REAL, INTENT(IN OUT)                     :: FEMIS(MXEMIS)
REAL, INTENT(IN OUT)                     :: XEMIS(MXEMIS)
REAL, INTENT(IN OUT)                     :: XRHO(MXEMIS)
REAL, INTENT(IN OUT)                     :: XCEMI1(MXEMIS)
REAL, INTENT(IN OUT)                     :: XCRHO1(MXEMIS)
REAL, INTENT(IN OUT)                     :: XCEMI2(MXEMIS)
REAL, INTENT(IN OUT)                     :: XCRHO2(MXEMIS)
LOGICAL, INTENT(IN OUT)                  :: LRHOT
REAL, INTENT(OUT)                        :: EMIS(MXCHAN)
REAL, INTENT(OUT)                        :: RHOSUN(MXCHAN)
REAL, INTENT(OUT)                        :: RHOTHR(MXCHAN)
REAL, INTENT(OUT)                        :: CEMIS1(MXCHAN)
REAL, INTENT(OUT)                        :: CRHOS1(MXCHAN)
REAL, INTENT(OUT)                        :: CRHOT1(MXCHAN)
REAL, INTENT(OUT)                        :: CEMIS2(MXCHAN)
REAL, INTENT(OUT)                        :: CRHOS2(MXCHAN)
REAL, INTENT(OUT)                        :: CRHOT2(MXCHAN)
!IMPLICIT NONE

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
INTEGER :: I  ! generic looping variable
INTEGER :: J  ! generic looping variable
INTEGER :: SORTED  ! conrol flag for sorts
REAL :: DELELM(MXEMIS)  ! delta for emis wavelength
REAL :: DELEMS(MXEMIS)  ! delta for emis
REAL :: DELRHO(MXEMIS)  ! delta for rho
REAL :: DELCE1(MXEMIS)  ! delta for cemis1
REAL :: DELCR1(MXEMIS)  ! delta for crho1
REAL :: DELCE2(MXEMIS)  ! delta for cemis2
REAL :: DELCR2(MXEMIS)  ! delta for crho2
REAL :: DX          ! delta X
REAL :: ELAM(MXEMIS)  ! emis wavelength
REAL :: RJUNK1  ! generic junk/work variable
REAL :: RJUNK2  ! generic junk/work variable
REAL :: RJUNK3  ! generic junk/work variable
REAL :: RJUNK4  ! generic junk/work variable
REAL :: RJUNK5  ! generic junk/work variable
REAL :: RJUNK6  ! generic junk/work variable
REAL :: RJUNK7  ! generic junk/work variable


!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none


!***********************************************************************
!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!***********************************************************************

!      ---------------------------
!      Sort the emis & rho by freq
!      ---------------------------
SORTED=1
10    IF (SORTED == 1) THEN
  SORTED=0
  DO I=1,NEMIS-1
    IF (FEMIS(I) > FEMIS(I+1)) THEN
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
    END IF
    ENDDO
      GO TO 10
    END IF
    
!      ----------------------------------------
!      Calc wavelength and interpolation deltas
!      ----------------------------------------
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
        
!      ------------------------------------
!      Interpolate emis & rho onto channels
!      ------------------------------------
!      Loop over channels (note: chan freqs may be in any order)
        DO I=1,NCHAN
          
!         Emissivity
          IF ( FREQ(I) <= FEMIS(1) ) THEN
            EMIS(I)=XEMIS(1)
            RHOSUN(I)=XRHO(1)
            CEMIS1(I)=XCEMI1(1)
            CRHOS1(I)=XCRHO1(1)
            CEMIS2(I)=XCEMI2(1)
            CRHOS2(I)=XCRHO2(1)
          ELSE IF ( FREQ(I) >= FEMIS(NEMIS) ) THEN
            EMIS(I)=XEMIS(NEMIS)
            RHOSUN(I)=XRHO(NEMIS)
            CEMIS1(I)=XCEMI1(NEMIS)
            CRHOS1(I)=XCRHO1(NEMIS)
            CEMIS2(I)=XCEMI2(NEMIS)
            CRHOS2(I)=XCRHO2(NEMIS)
          ELSE
!            Determine the index of the upper bounding FEMIS
            J=2
            20          IF ( FEMIS(J) < FREQ(I) ) THEN
              J=J + 1
              GO TO 20
            END IF
!            Convert to lower boundary index
            J=J - 1
            
            DX=(( 1.0E+4/FREQ(I) - ELAM(J) )/DELELM(J))
            EMIS(I) = XEMIS(J) + DX*DELEMS(J)
            RHOSUN(I) = XRHO(J) + DX*DELRHO(J)
            CEMIS1(I) = XCEMI1(J) + DX*DELCE1(J)
            CRHOS1(I) = XCRHO1(J) + DX*DELCR1(J)
            CEMIS2(I) = XCEMI2(J) + DX*DELCE2(J)
            CRHOS2(I) = XCRHO2(J) + DX*DELCR2(J)
          END IF
          
          IF (LRHOT) THEN
!            Force reflected thermal rho = (1-emis)/pi
            RHOTHR(I)=(1.0 - EMIS(I))/PI
            CRHOT1(I)=(1.0 - CEMIS1(I))/PI
            CRHOT2(I)=(1.0 - CEMIS2(I))/PI
          ELSE
!            Reflected thermal uses the same rho as solar
            RHOTHR(I)=RHOSUN(I)
            CRHOT1(I)=CRHOS1(I)
            CRHOT2(I)=CRHOS1(I)
          END IF
          
          ENDDO
            
            RETURN
          END SUBROUTINE SETEMS

END MODULE SET_EMS_PCLSAM
